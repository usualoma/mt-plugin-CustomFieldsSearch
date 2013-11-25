#   Copyright (c) 2008 ToI-Planning, All rights reserved.
# 
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
# 
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#   2. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#
#   3. Neither the name of the authors nor the names of its contributors
#      may be used to endorse or promote products derived from this
#      software without specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  $Id$

package CustomFieldsSearch::App;

use strict;
use warnings;

my %tag_field = qw(
	entrytitle title
	entrybody text
	entrymore text_more
	entrykeywords keywords
);

sub enabled {
    my ($app) = @_;
    $app ||= MT->instance;
    $app->can('param') && $app->param('CustomFieldsSearch');
}

sub is_empty_search {
    my ($app) = @_;
    $app ||= MT->instance;
    !$app->param('searchTerms') && !$app->param('search');
}

sub _hdlr_no_search_results {
    my ( $ctx, $args, $cond ) = @_;
    return 1 unless is_empty_search();
    $ctx->stash('count') ? 0 : 1;
}

sub init_app {
    my ( $cb, $app ) = @_;

    require MT::App::Search;

    local $SIG{__WARN__} = sub { };
    if ( $MT::VERSION < 4.2 && enabled($app) ) {
        my @fields = grep( {$_} $app->param('CustomFieldsSearchField') );
        if ( !@fields ) {
            my $cf = MT->component('CustomFields');
            $cf->{search_hit_method} = \&MT::App::Search::_search_hit;
            *MT::App::Search::_search_hit = sub {
                require CustomFields::App::Search;
                my $method_ref
                    = CustomFields::App::Search->can('_search_hit');
                return $method_ref->( $cf, @_ );
            };
        }
        else {
            my $hit_method = \&MT::App::Search::_search_hit;
            *MT::App::Search::_search_hit = sub {
                return &_search_hit( \@fields, $hit_method, @_ );
            };
        }
    }
    else {
        my $query_parse = \&MT::App::Search::query_parse;
        *MT::App::Search::query_parse = sub {
            enabled($app)
                ? &query_parse( is_empty_search($app), @_ )
                : $query_parse->(@_);
        };

        my $execute = \&MT::App::Search::execute;
        *MT::App::Search::execute = sub {
            enabled($app)
                ? &execute( $execute, is_empty_search($app), @_ )
                : $execute->(@_);
        };

        require MT::Template::Context::Search;
        my $context_script = \&MT::Template::Context::Search::context_script;
        *MT::Template::Context::Search::context_script = sub {
            enabled($app) ? context_script(@_) : $context_script->(@_);
        };
    }
}

sub init_request {
    my ( $plugin, $app ) = @_;

    if ( $app->isa('MT::App::Search') && enabled($app) ) {
        if ( $MT::VERSION >= 4.2 ) {
            if ( is_empty_search($app) ) {
                $app->param( 'search', 'CustomFieldsSearch' );
            }
        }
    }
}

sub context_script {
	my ( $ctx, $args, $cond ) = @_;

	require MT;
	my $app = MT->instance;

	my $cgipath = ($ctx->handler_for('CGIPath'))[0]->($ctx, $args);
	my $script = $ctx->{config}->SearchScript;

	my @ignores = ('startIndex', 'limit', 'offset', 'format', 'page');
	my $q = new CGI('');
	if ($app->isa('MT::App::Search')) {
		foreach my $p ($app->param) {
			if (! grep({ $_ eq $p } @ignores)) {
				$q->param($p, $app->param($p));
			}
		}
	}

	local $CGI::USE_PARAM_SEMICOLONS;
	$CGI::USE_PARAM_SEMICOLONS = 0;
	$cgipath . $script . '?' . $q->query_string;
}

sub feelingLucky {
	my ($app) = @_;
	$app->param('CustomFieldsSearchLucky') || 0;
}

sub found {
	my ($app, $entry) = @_;
	if (&feelingLucky($app)) {
		my $at = $app->param('CustomFieldsSearchLuckyArchiveType') || '';
		my $url = $entry->permalink($at);
		$app->redirect($url);
		return 0;
	}
	return 1;
}

sub execute {
	my $execute = shift;
	my $empty_search = shift;
	my ($app, $terms, $args ) = @_;

	if ($empty_search) {
		$app->{search_string} = '';
		$app->param('search', '');
	}

	if (
		$app->param('CustomFieldsSearchSort')
		&& ! (
			$app->param('SearchSortBy') || $app->param('SearchResultDisplay')
		)
		&& $args->{'join'}
	) {
		delete($args->{'sort'});
	}

	my ($count, $iter) = $execute->(@_);
	if ($count && &feelingLucky($app)) {
		my $entry = $iter->();
		&found($app, $entry);
	}
	else {
		($count, $iter);
	}
}

sub field_params {
	my $app = shift;
	my %params = ();
	my $orig = $app->param->Vars;

	foreach my $k (keys(%$orig)) {
		my ($pkey) = ($k =~ m/([^:]*)/);
		foreach my $pk (split(/,/, $pkey)) {
			push(@{ $params{$pk} ||= [] }, split("\0", $orig->{$k}));
		}
	}

	\%params;
}

sub query_parse {
	my $empty_search = shift;
	my $app = shift;
	my ( %columns ) = @_;
	my $terms = [];
	my $args = {};
	my @and_ids = ();
	my $blog_ids = undef;
	if (ref $app->{searchparam}{IncludeBlogs} eq 'HASH') {
		$blog_ids = [ keys %{ $app->{searchparam}{IncludeBlogs} } ];
	}
	else {
		$blog_ids = $app->{searchparam}{IncludeBlogs};
	}
	my $plugin = $app->component('CustomFieldsSearch');

	my @class_types = $app->param('CustomFieldsSearchClassType');
	if (! @class_types) {
		@class_types = ('entry', 'page');
	}

	my $smart_string = $app->param('CustomFieldsSearchSmart') || 0;
	my $smart_splitter =
		$app->param('CustomFieldsSearchSmartSplitter') || '\s';
	my $smart_enclosure =
		$app->param('CustomFieldsSearchSmartEnclosure') || '"';
	my @search_strings;
	my @search_strings_ids;
	if ($smart_string) {
		my $search = $app->{search_string};
		while ($search =~ m/
			$smart_enclosure([^$smart_enclosure]+?)$smart_enclosure|
			(.*?)(?:$smart_splitter|\z)
		/xmsg) {
			my $str = $1 || $2;
			push(@search_strings, $str) if $str;
		}
	}
	else {
		push(@search_strings, $app->{search_string});
	}

	my $field_params = field_params($app);

	if (my $fields = $field_params->{'CustomFieldsSearchDateTimeField'}) {
		for my $field (@$fields) {
			for my $f (split(',', $field)) {
				my @values = ();
				for my $i (1..6) {
					push(
						@values,
						$field_params->{"$f($i)"} ?
							join('', @{ $field_params->{"$f($i)"} }) : undef
					);
				}
				if ($values[0] && $values[1] && $values[2]) {
					$field_params->{$f} = [sprintf(
						'%04d-%02d-%02d %02d:%02d:%02d', @values
					)];
				}
			}
		}
	}

	if (my $aliases = $field_params->{'CustomFieldsSearchAlias'}) {
		for my $alias (@$aliases) {
			for my $a (split(',', $alias)) {
                my ($to, $from) = split(':', $a);
                if ($to && $from) {
                    $field_params->{$to} = $field_params->{$from};
                }
			}
		}
	}

	# CustomFields field matching.
	my @fields = grep({ $_ } $app->param('CustomFieldsSearchField'));

	my (%likes, %equals, %ins, %not_ins) = ();
	my (@like_tags, @equals_tags, @in_tags, @not_in_tags) = ();
	foreach my $tuple (
		['Like', \%likes, \@like_tags, 0],
		['Equals', \%equals, \@equals_tags, 0],
		['In', \%ins, \@in_tags, 1],
		['NotIn', \%not_ins, \@not_in_tags, 1]
	) {
		my ($key, $hash, $keys, $is_array) = @$tuple;
		map({
			my %map;

			if ($_ =~ m/^([\w,]+):(.*)/) {
				my ($m1, $m2) = ($1, $2);
				foreach my $k (split(/,/, $m1)) {
					$map{$k} = $m2;
				}
			}
			else {
				foreach my $k (split(/,/, $_)) {
					if ($field_params->{$k}) {
						$map{$k} = join(',', @{ $field_params->{$k} }) || undef;
					}
				}
			}

			while (my ($k, $v) = each(%map)) {
				if ($k && defined($v)) {
					if ($is_array) {
						$hash->{$k} ||= [];
						push(@{ $hash->{$k} }, split(',', $v));
					}
					else {
						$hash->{$k} = $v;
					}
				}
			}
		} @{ $field_params->{'CustomFieldsSearchField' . $key} });
		@$keys = keys(%$hash);
	}

	my %ranges = ();
	my %range_tags = ();
	foreach (@{ $field_params->{'CustomFieldsSearchFieldRange'} }) {
		if ($_ =~ m/^(\w+):(.*)/) {
			my $tag = $1;
			foreach (split(',', $2)) {
				if (m/([<>=]+)([\d\s\/:-]*)/) {
					my ($op, $value) = ($1, $2);
					my $op_orig = $op;
					# "=>" is replaced to ">=" and "=<" is replaced to "<="
					$op =~ s/=(<|>)/$1=/;
					$ranges{$op} ||= {};
					$ranges{$op}{$tag} ||= [];
					if (! defined($value) || $value eq '') {
						my $values =
							$field_params->{$tag} ||
							$field_params->{"$tag:$op_orig"};
						if ($values) {
							$value = join(',', @$values);
						}
						if (! defined($value) || $value eq '') {
							$value = undef;
						}
					}
					push(@{ $ranges{$op}{$tag} }, $value) if defined($value);
				}
			}
		}
	}
	while (my($k, $h) = each(%ranges)) {
		$range_tags{$k} = [ keys(%$h) ];
	}

	my $obj_type = $app->{searchparam}{Type};

	require CustomFields::Field;
	require CustomFields::App::CMS;

	my $meta_terms = [];
	push(@$meta_terms, []) for (1..scalar(@search_strings));
	my $meta_terms_ands = {};
	my $types = $app->registry('customfield_types');

	my $field_terms = {
		obj_type => \@class_types,
		blog_id => [@$blog_ids, 0],
	};

	$field_terms->{'tag'} = [
		@fields, @like_tags, @equals_tags, @in_tags, @not_in_tags,
		map(@$_, values(%range_tags))
	];
	$plugin->{target_tags} = {};
	foreach my $hash (\%likes, \%equals, \%ins, \%not_ins) {
		while (my($tag, $value) = each(%$hash)) {
			$plugin->{target_tags}{lc($tag)} = ref $value ? $value : [ $value ];
		}
	}
	foreach my $op (reverse(sort(keys(%ranges)))) {
		my $hash = $ranges{$op};
		while (my($tag, $value) = each(%$hash)) {
			$tag = lc($tag);
			$plugin->{target_tags}{$tag} ||= [];
			push(
				@{ $plugin->{target_tags}{$tag} },
				$op . ' ' . join(',', @$value)
			);
		}
	}

	if (! @{ $field_terms->{'tag'} }) {
		delete($field_terms->{'tag'});
	}

	my @c_fields = CustomFields::Field->load($field_terms);

	foreach my $f (@c_fields) {
		if (! $types->{$f->type}) {
			next;
		}

		my $tag = $f->tag;

		if (! $empty_search) {
			if (grep({ $_ eq $tag } @fields)) {
				for (my $i = 0; $i < scalar(@search_strings); $i++) {
					push(@{ $meta_terms->[$i] }, '-or', [
						{
							'type' => 'field.' . $f->basename,
						},
						'-and',
						{
							$types->{$f->type}->{'column_def'} => {
								'like' => '%' . $search_strings[$i] . '%',
							},
						},
					]);
				}
			}
		}

		if (grep({ $_ eq $tag } @like_tags)) {
			$meta_terms_ands->{"CustomFieldsSearchFieldLike:$tag"} = [
				{
					'type' => 'field.' . $f->basename,
				},
				'-and',
				{
					$types->{$f->type}->{'column_def'} => {
						'like' => '%' . $likes{$tag} . '%',
					},
				},
			];
		}

		foreach my $tuple (
			[\@equals_tags, \%equals, 'Equals'], [\@in_tags, \%ins, 'In']
		) {
			my ($tags, $hash, $key) = @$tuple;
			if (grep({ $_ eq $tag } @$tags)) {
				$meta_terms_ands->{"CustomFieldsSearchField$key:$tag"} = [
					{
						'type' => 'field.' . $f->basename,
					},
					'-and',
					{
						$types->{$f->type}->{'column_def'} => $hash->{$tag},
					},
				];
			}
		}
		if (grep({ $_ eq $tag } @not_in_tags)) {
			$meta_terms_ands->{"CustomFieldsSearchFieldNotIn:$tag"} = [
				{
					'type' => 'field.' . $f->basename,
				},
				'-and',
				{
					$types->{$f->type}->{'column_def'} => {
						not => $not_ins{$tag},
					},
				},
			];
		}

		foreach my $tuple (
			['<', 'range', 1],
			['<=', 'range_incl', 1],
			['>', 'range', 0],
			['>=', 'range_incl', 0],
		) {
			my ($op, $type, $index) = @$tuple;
			my $tags = $range_tags{$op};
			if (grep({ $_ eq $tag } @$tags)) {
				foreach my $v (@{ $ranges{$op}{$tag} }) {
					my @range = (undef, undef);
					$range[$index] = $v;
					$meta_terms_ands->{"CustomFieldsSearchFieldRange:$op:$tag"} = {
						'terms' => [
							{
								'type' => 'field.' . $f->basename,
							},
							'-and',
							{
								$types->{$f->type}->{'column_def'} => \@range
							},
						],
						'args' => {
							$type => {
								$types->{$f->type}->{'column_def'} => 1,
							},
						},
					};
				}
			}
		}
	}
	shift(@$_) for @$meta_terms;

	my $obj_class = $app->model($obj_type);
	my $obj_id_key = $obj_class->datasource . '_id';
	my $meta_pkg = $obj_class->meta_pkg;

	if (%$meta_terms_ands) {
		my $init = 0;
		my @ids = ();

		my @groups = ();
		if (my $gs = $field_params->{'CustomFieldsSearchFieldGroup'}) {
			foreach my $g (@$gs) {
				my @g = split(/,/, $g);
				my @group = ();
				foreach my $k (keys(%$meta_terms_ands)) {
					if (grep($k eq $_, @g)) {
						push(@group, delete($meta_terms_ands->{$k}));
					}
				}
				push(@groups, \@group) if @group;
			}
		}
		push(@groups, map([$_], values(%$meta_terms_ands)));

		foreach my $g (@groups) {
			my %ids = ();
			foreach my $terms (@$g) {
				my $args = {fetchonly => [ $obj_id_key ]};
				if (ref($terms) eq 'HASH' && $terms->{'terms'}) {
					$args = {%$args, %{ $terms->{'args'} }} if $terms->{'args'};
					$terms = $terms->{'terms'};
				}

				my $iter = $meta_pkg->search(
					$terms, $args
				);
				while (my $e = $iter->()) {
					$ids{$e->$obj_id_key} = 1;
				}
			}

			if (! %ids) {
				return { terms => { id => 0 } };
			}

			if (! $init) {
				@ids = keys(%ids);
			}
			else {
				@ids = grep(
					{ my $i = $_; grep({ $i == $_ } @ids) }
					keys(%ids)
				);
			}
			$init = 1;
		}

		if (! @ids) {
			return { terms => { id => 0 } };
		}

		if ($empty_search || @{ $meta_terms->[0] }) {
			for (my $i = 0; $i < scalar(@$meta_terms); $i++) {
				if (@{ $meta_terms->[$i] }) {
					$meta_terms->[$i] = [ $meta_terms->[$i], '-and' ];
				}
				push(@{ $meta_terms->[$i] }, { $obj_id_key => \@ids });

			}
		}
		@and_ids = @ids;
	}

	if ($empty_search || @{ $meta_terms->[0] }) {
		my %ids = ();

		if ($empty_search) {
			$ids{$_} = 1 for @and_ids;
		}
		else {
			for (my $i = 0; $i < scalar(@$meta_terms); $i++) {
				my $iter = $meta_pkg->search(
					$meta_terms->[$i], {fetchonly => [ $obj_id_key ]}
				);
				my (%tmp);
				while (my $e = $iter->()) {
					$tmp{$e->$obj_id_key} = 1;
				}

				$search_strings_ids[$i] = [ keys(%tmp) ];

				if ($i == 0) {
					%ids = %tmp;
				}
				else {
					foreach my $k (keys(%ids)) {
						delete($ids{$k}) unless $tmp{$k};
					}
				}
			}
		}

		if (%ids) {
			push(@$terms, (scalar(@$terms) ? '-or' : ()),
				{
					'id' => [ keys %ids ],
					'class' => \@class_types,
					'blog_id' => $blog_ids,
				}
			);
		}
	}

	# Default field matching.

	my @ignores = map({
		$_ =~ s/^mt:?//i; $_
	} $app->param('CustomFieldsSearchIgnore'));
	foreach my $tag (keys(%tag_field)) {
		if (
			(grep({ lc($_) eq $tag } @ignores))
			&& (! grep({ lc($_) eq $tag } @fields))
		) {
			delete $columns{$tag_field{$tag}};
		}
		else {
			$plugin->{target_tags}{lc($tag)} = \@search_strings;
		}
	}

	my @column_names = keys %columns;

	if (! $empty_search && @column_names) {
		my $tmps = [];
		push(@$tmps, []) for (1..scalar(@search_strings));
		foreach my $c (@column_names) {
			for (my $i = 0; $i < scalar(@search_strings); $i++) {
				push(@{ $tmps->[$i] }, '-or', [
					(($search_strings_ids[$i] && @{ $search_strings_ids[$i] })
						?  ( { 'id' => $search_strings_ids[$i] }, '-or' )
						: ()
					),
					{ $c => {
						'like' => '%' . $search_strings[$i] . '%',
					}}
				]);
			}
		}
		shift(@$_) for @$tmps;

		my @tmp_ands = ();
		foreach (@$tmps) {
			push(@tmp_ands, '-and', $_);
		}

		push(@$terms, (scalar(@$terms) ? '-or' : ()),
			[
				{
					(@and_ids ? ('id' => \@and_ids) : ()),
					'class' => \@class_types,
					'blog_id' => $blog_ids,
				},
				@tmp_ands,
			]
		);
	}

	if (! @$terms) {
		if (! $app->param('PreventEmptySearch')) {
			$terms = [{
				'class' => \@class_types,
				'blog_id' => $blog_ids,
			}];
		}
		else {
			$terms = [{
				'class' => \@class_types,
				'blog_id' => 0,
			}];
		}
		$app->{custom_fields_no_search} = 1;
	}
	else {
		$app->{custom_fields_no_search} = 0;
	}

	if (my $sorts = $app->param('CustomFieldsSearchSort')) {
		$sorts =~ s/^\s*|\s*$//g;
		my @params = ();
		foreach my $sort (split(/\s*,\s*/, $sorts)) {
			my @param = split(/\s+/, $sort);
			if (
				(scalar(@param) == 1)
				|| (($param[1] ne 'descend') && ($param[1] ne 'DESC'))
			) {
				$param[1] = 'ascend';
			}
			else {
				$param[1] = 'descend';
			}
			push(@params, \@param);
		}

		my $field_terms = {
			obj_type => \@class_types,
			blog_id => [@$blog_ids, 0],
			tag => [ map($_->[0], @params) ],
		};
		my @fields = CustomFields::Field->load($field_terms);

		my @joins = ();
		foreach my $param (@params) {
			my ($f) = grep($_->tag eq $param->[0], @fields);
			if ($f) {
				push(@joins, [ $meta_pkg, 'entry_id',
					{
						'type' => 'field.' . $f->basename,
					},
					{
						'sort' => $types->{$f->type}->{'column_def'},
						'direction' => $param->[1],
					}
				]);
			}
		}

		$args->{'join'} = shift(@joins) if @joins;
	}

	{ terms => $terms, args => $args };
}

sub _search_hit {
	my ($fields, $hit_method, $app, $entry) = @_;

	my @ignores = map({
		$_ =~ s/^mt:?//i; $_
	} $app->param('CustomFieldsSearchIgnore'));
	if (@ignores) {
		my $str = '';
		foreach my $tag (keys(%tag_field)) {
			if (
				(! grep({ lc($_) eq $tag } @ignores))
				|| (grep({ lc($_) eq $tag } @$fields))
			) {
				my $field = $tag_field{$tag};
				$str .= $entry->$field;
			}
		}

		return 1 if $app->is_a_match($str);
	}
	else {
		return 1 if &{$hit_method}($app, $entry);
		return 0 if $app->{searchparam}{SearchElement} ne 'entries';
	}

	my $meta = CustomFields::Util::get_meta($entry);

	require CustomFields::Field;
	my $terms = {
		obj_type => 'entry',
		tag => $fields,
	};
	my @fields = CustomFields::Field->load($terms);

	my @text_elements = map({
		$meta->{$_->basename};
	} @fields);

	return 1 if $app->is_a_match(join("\n", map $_ || '', @text_elements));
}

1;

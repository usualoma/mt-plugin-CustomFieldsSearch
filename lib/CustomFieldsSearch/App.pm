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

my %tag_field = qw(
	entrytitle title
	entrybody text
	entrymore text_more
	entrykeywords keywords
);

sub init_request {
	my ($plugin, $app) = @_;

	if ($app->isa('MT::App::Search')) {
		my $enable = $app->param('CustomFieldsSearch');

		if ($enable) {
			local $SIG{__WARN__} = sub {  }; 
			if ($MT::VERSION < 4.2) {
				my @fields = grep(
					{ $_ } $app->param('CustomFieldsSearchField')
				);
				if (! @fields) {
					my $cf = MT->component('CustomFields');
					$cf->{search_hit_method} = \&MT::App::Search::_search_hit;
					*MT::App::Search::_search_hit = sub {
						require CustomFields::App::Search;
						my $method_ref = CustomFields::App::Search->can(
							'_search_hit'
						);
						return $method_ref->($cf, @_);
					};
				}
				else {
					my $hit_method = \&MT::App::Search::_search_hit;
					*MT::App::Search::_search_hit = sub {
						return &_search_hit(\@fields, $hit_method, @_);
					};
				}
			}
			else {
				my $empty_search = 0;
				if (! $app->param('searchTerms') && ! $app->param('search')) {
					$empty_search = 1;
					$app->param('search', 'CustomFieldsSearch');
				}

				my $query_parse = \&MT::App::Search::query_parse;
				*MT::App::Search::query_parse = sub {
					return &query_parse($empty_search, @_);
				};

				my $execute = \&MT::App::Search::execute;
				*MT::App::Search::execute = sub {
					return &execute($execute, $empty_search, @_);
				};

				require MT::Template::Context::Search;
				my $context_script = \&MT::Template::Context::Search::context_script;
				*MT::Template::Context::Search::context_script = \&context_script;

			}
		}
	}
}

sub context_script {
	my ( $ctx, $args, $cond ) = @_;

	require MT;
	my $app = MT->instance;

    my $cgipath = $ctx->_hdlr_cgi_path($args);
    my $script = $ctx->{config}->SearchScript;

	my @ignores = ('startIndex', 'limit', 'offset', 'format');
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

	my ($count, $iter) = $execute->(@_);
	if ($count && &feelingLucky($app)) {
		my $entry = $iter->();
		&found($app, $entry);
	}
	else {
		($count, $iter);
	}
}

sub query_parse {
	my $empty_search = shift;
    my $app = shift;
    my ( %columns ) = @_;
	my $terms = [];
	my @and_ids = ();
	my $blog_ids = [ keys %{ $app->{searchparam}{IncludeBlogs} } ];
	my $plugin = $app->component('CustomFieldsSearch');

	my @class_types = $app->param('CustomFieldsSearchClassType');
	if (! @class_types) {
		@class_types = ('entry', 'page');
	}

	# CustomFields field matching.
	my @fields = grep({ $_ } $app->param('CustomFieldsSearchField'));

	my (%likes, %equals, %ins) = ();
	my (@like_tags, @equals_tags, @in_tags) = ();
	foreach my $tuple (
		['Like', \%likes, \@like_tags, 0],
		['Equals', \%equals, \@equals_tags, 0],
		['In', \%ins, \@in_tags, 1]
	) {
		my ($key, $hash, $keys, $is_array) = @$tuple;
		map({
			if ($_ =~ m/^(\w+):(.*)/) {
				if ($is_array) {
					$hash->{$1} ||= [];
					push(@{ $hash->{$1} }, $2);
				}
				else {
					$hash->{$1} = $2;
				}
			}
		} $app->param('CustomFieldsSearchField' . $key));
		@$keys = keys(%$hash);
	}

	my $obj_type = $app->{searchparam}{Type};

	require CustomFields::Field;
	require CustomFields::App::CMS;

	my $meta_terms = [];
	my $meta_terms_ands = [];
	my $types = CustomFields::App::CMS->load_customfield_types;

	my $field_terms = {
		obj_type => $obj_type,
		blog_id => $blog_ids,
	};

	$field_terms->{'tag'} = [ @fields, @like_tags, @equals_tags, @in_tags ];
	$plugin->{target_tags} = [ @{ $field_terms->{'tag'} } ];
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
				push(@$meta_terms, '-or', [
					{
						'type' => 'field.' . $f->basename,
					},
					'-and',
					{
						$types->{$f->type}->{'column_def'} => {
							'like' => '%' . $app->{search_string} . '%',
						},
					},
				]);
			}
		}

		if (grep({ $_ eq $tag } @like_tags)) {
			push(@$meta_terms_ands, [
				{
					'type' => 'field.' . $f->basename,
				},
				'-and',
				{
					$types->{$f->type}->{'column_def'} => {
						'like' => '%' . $likes{$tag} . '%',
					},
				},
			]);
		}

		foreach my $tuple (
			[\@equals_tags, \%equals], [\@in_tags, \%ins]
		) {
			my ($tags, $hash) = @$tuple;
			if (grep({ $_ eq $tag } @$tags)) {
				push(@$meta_terms_ands, [
					{
						'type' => 'field.' . $f->basename,
					},
					'-and',
					{
						$types->{$f->type}->{'column_def'} => $hash->{$tag},
					},
				]);
			}
		}
	}
	shift(@$meta_terms);

	my $obj_class = $app->model($obj_type);
	my $obj_id_key = $obj_class->datasource . '_id';
	my $meta_pkg = $obj_class->meta_pkg;

	if (@$meta_terms_ands) {
		my $init = 0;
		my @ids = ();
		foreach my $terms (@$meta_terms_ands) {
			my $iter = $meta_pkg->search(
				$terms, {fetchonly => [ $obj_id_key ]}
			);
			my %ids = ();
			while (my $e = $iter->()) {
				$ids{$e->$obj_id_key} = 1;
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

		if ($empty_search || @$meta_terms) {
			push(@$meta_terms,
				(@$meta_terms ? '-and' : ()),
				{ $obj_id_key => \@ids }
			);
		}
		@and_ids = @ids;
	}

	if ($empty_search || @$meta_terms) {
		my $iter = $meta_pkg->search(
			$meta_terms, {fetchonly => [ $obj_id_key ]}
		);
		my %ids = ();
		while (my $e = $iter->()) {
			$ids{$e->$obj_id_key} = 1;
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

	my @ignores = $app->param('CustomFieldsSearchIgnore');
	foreach my $tag (keys(%tag_field)) {
		if (
			(grep({ lc($_) eq $tag } @ignores))
			&& (! grep({ lc($_) eq $tag } @fields))
		) {
			delete $columns{$tag_field{$tag}};
		}
		else {
			push(@{ $plugin->{target_tags} }, $tag);
		}
	}

	my @column_names = keys %columns;

	if (! $empty_search && @column_names) {
		my $tmp = [];
		foreach my $c (@column_names) {
			push(@$tmp, '-or', { $c => {
				'like' => '%' . $app->{search_string} . '%',
			}});
		}
		shift(@$tmp);

		push(@$terms, (scalar(@$terms) ? '-or' : ()),
			[
				{
					(@and_ids ? ('id' => \@and_ids) : ()),
					'class' => \@class_types,
					'blog_id' => $blog_ids,
				},
				'-and',
				$tmp
			]
		);
	}

	if (! @$terms) {
		$terms = [{
			'class' => \@class_types,
			'blog_id' => $blog_ids,
		}];
	}

	{ terms => $terms, args => {} };
}

sub _search_hit {
    my ($fields, $hit_method, $app, $entry) = @_;

	my @ignores = $app->param('CustomFieldsSearchIgnore');
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

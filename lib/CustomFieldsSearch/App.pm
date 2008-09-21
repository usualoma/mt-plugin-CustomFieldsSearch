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

sub init_request {
	my ($plugin, $app) = @_;

	if ($app->isa('MT::App::Search')) {
		my $enable = $app->param('CustomFieldsSearch');
		my @fields = $app->param('CustomFieldsSearchField');

		if ($enable) {
			local $SIG{__WARN__} = sub {  }; 
			if ($MT::VERSION < 4.2) {
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
				my $execute = \&MT::App::Search::execute;
				*MT::App::Search::execute = sub {
					return &execute($execute, \@fields, @_);
				};
			}
		}
	}
}

sub execute {
	my $execute = shift;
	my $fields = shift;
	my ($app, $terms, $args ) = @_;

	require CustomFields::Field;
	require CustomFields::App::CMS;
	require MT::Entry;

	my %terms = (
		obj_type => 'entry',
	);

	if (@$fields) {
		$terms{'tag'} = $fields;
	}
	my @c_fields = CustomFields::Field->load(\%terms);

	my $types = CustomFields::App::CMS->load_customfield_types;
	my @terms = ();
	foreach my $f (@c_fields) {
		if (! $types->{$f->type}) {
			next;
		}

		push(@terms, (scalar(@terms) ? '-or' : ()), [
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

	my $meta_pkg = MT::Entry->meta_pkg;
	my $iter = $meta_pkg->search(\@terms, {fetchonly => [ 'entry_id' ]});
	my %ids = ();
	while (my $e = $iter->()) {
		$ids{$e->entry_id} = 1;
	}

	if (%ids) {
		for (my $i = scalar(@$terms); $i >= 0; $i--) {
			if ((ref $terms->[$i]) eq 'ARRAY') {
				push(@{ $terms->[$i] }, '-or', {
					'id' => [ keys %ids ],
				});
			}
		}
	}

	return $execute->(@_);
}

sub _search_hit {
    my ($fields, $hit_method, $app, $entry) = @_;

    return 1 if &{$hit_method}($app, $entry);
    return 0 if $app->{searchparam}{SearchElement} ne 'entries';

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

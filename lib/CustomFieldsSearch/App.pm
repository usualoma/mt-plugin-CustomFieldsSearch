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
use CustomFields::Util qw( get_meta );

sub init_request {
	my ($plugin, $app) = @_;

	if ($app->isa('MT::App::Search')) {
		my $enable = $app->param('CustomFieldsSearch');
		my @fields = $app->param('CustomFieldsSearchField');

		local $SIG{__WARN__} = sub {  }; 
		if ($enable) {
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
	}
}

sub _search_hit {
    my ($fields, $hit_method, $app, $entry) = @_;

    return 1 if &{$hit_method}($app, $entry); # If query matches non-CustomFields, why waste time?
    return 0 if $app->{searchparam}{SearchElement} ne 'entries'; # If it hasn't matched and isn't searching on entries, again why waste time?

	my $meta = get_meta($entry);

    require CustomFields::Field;
	my $terms = {
		obj_type => 'entry',
		#obj_type => $obj_type,
		#blog_id => ( $blog_id ? [ $blog_id, 0 ] : 0 ),
		tag => $fields,
    };
	my @fields = CustomFields::Field->load($terms);

    my @text_elements = map({
		$meta->{$_->basename};
	} @fields);

    return 1 if $app->is_a_match(join("\n", map $_ || '', @text_elements));
}

1;

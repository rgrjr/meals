###================
#
# A recipe is a food item that is itself made from one or more ingredients,
# which refer to other recipes or food items.  The food values are stated per
# serving, and are computed once from the ingredient food values.
#

use strict;
use warnings;

package Food::Recipe;

use base qw(Food::HasIngredients);

BEGIN {
    Food::Recipe->define_class_slots(qw(n_servings));
}

1;

# meals.pl testing.  -- rgr, 24-Jan-15.

test:	test-show1 test-day1 test-day2 test-day3 test-day4

test-day1:
	./meals.pl day1.text > $@.tmp
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day2:
	./meals.pl day2.text > $@.tmp
	cmp $@.tbl $@.tmp
	rm -f $@.tmp
test-day3:
	./meals.pl --det day3.text > $@.tmp 2>&1
	cmp $@.text $@.tmp
	rm -f $@.tmp
test-day4:
	./meals.pl --det day4.text > $@.tmp
	cmp $@.text $@.tmp
	rm -f $@.tmp
test-show1:
	./meals.pl --show "Jan's pancakes" > $@.tmp
	cmp $@.text $@.tmp
	rm -f $@.tmp

# "Production" targets.
all-plots =     2014-Dec-calories.png 2014-Dec-carbs.png \
		2015-Jan-calories.png 2015-Jan-carbs.png \
		2015-Feb-calories.png 2015-Feb-carbs.png
all:	${all-plots}
2014-Dec-calories.png:	1412-food.text
	./meals.pl $^ --plot-cal $@
2014-Dec-carbs.png:	1412-food.text
	./meals.pl $^ --plot-cho $@
2015-Jan-calories.png:	1501-food.text
	./meals.pl $^ --plot-cal $@
2015-Jan-carbs.png:	1501-food.text
	./meals.pl $^ --plot-cho $@
2015-Feb-calories.png:	1502-food.text
	./meals.pl $^ --plot-cal $@
2015-Feb-carbs.png:	1502-food.text
	./meals.pl $^ --plot-cho $@
# For ephemeral use, not on all-plots.
recent:	recent-calories.png recent-carbs.png
recent-food.text:
	cat 1501-food.text 1502-food.text > $@
recent-calories.png:	recent-food.text
	./meals.pl $^ --plot-cal $@
recent-carbs.png:	recent-food.text
	./meals.pl $^ --plot-cho $@

clean-recent:
	rm -f recent-calories.png recent-carbs.png recent-food.text
clean:	clean-recent
	rm -f ${all-plots}

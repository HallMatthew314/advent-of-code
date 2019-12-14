# Confessions

**READER BEWARE: THIS FILE CONTAINS SPOILERS FOR SOME OF THE PROBLEMS IN ADVENT OF CODE.
THEY ARE PLACED INSIDE** `details` **TAGS BUT YOUR BROWSER MAY NOT SUPPORT THIS, LEAVING THEM VISIBLE.
READ WITH CAUTION.**

While all of the code in this repository is written by me, I cannot claim that all solutions presented are entirely my own.
What do I mean by this?
If I felt particularly stuck with a challenge, I would do a very small amount of research on other people's solutions, typically on the [Advent of Code subreddit](https://old.reddit.com/r/adventofcode).
While doing so, I would avoid looking at actual code unless I *absolutely* needed to.
Instead, I tried to only read written descriptions of how other solutions worked to see I there was a piece of the puzzle I had overlooked.
I maintain that I only did this to attempt to see the problems from different angles, that I might understand them better.
Below are the problems that I have researched and what I learned from doing so.

<details>
	<summary>2015 - Day 7 Part 1</summary>
	My first major leap in progress came after I realised I could use a similar approach for this problem as I did for 2019 Day 6.
	After I implemented the hash in Ruby I ran the program, but it seemed to be taking longer than it should.
	I suspected that I was dealing with circular dependancies, as the input data is essentially a directional graph.
	I was very frustrated at this and decided to search for answers when I came across the old Daily Solutions Megathread on the subreddit.
	There I learned that after calculating a wire's value, I could replace the expression to calculate the value in the hash with the expression's result.
	I implemented this and it worked.
	After doing some more research after the fact, it seems that I was not the only person who ran into this problem.
</details>

<details>
	<summary>2019 - Day 8 Part 2</summary>
	I intially found this problem to be worded problematically, as the phrase <q cite="https://adventofcode.com/2019/day/8">What message is produced after decoding your image?</q> is somewhat vague.
	This question does not give any sort of indication as to what kind of message is being sent.
	One could only infer that it could fit in a black-and-white bitmap of size 25x6.
	This lack of information made it troublesome to debug my solution.
	I was confident that the process I had devised would work, but I wasn't getting anything meaningful when I ran my code.
	After checking the subreddit, I saw some example outputs and realised what the problem was.
	The height of the example solutions I saw was 6, which was also stated in the proble, description.
	However, I had declared the height to be 8 in my code.
	I changed this to 6 and the output was much more legible, as well as providing the correct solution.
</details>

<details>
	<summary>2019 - Day 10 Part 2</summary>
	I tired looking at a lot of clues for this one, but none of them were able to help me.
	Out of desperation, I eventually tried changing the index offset to be calculated from `Math::PI` instead of `Math::PI/2`.
	I found this unusual, since in the standard was of measuring angles in mathematics, 'straight up' or 0 degrees in the puzzle <em>should</em> be equal to half-pi radians.
	A short while later, I realised my function for calculating angles was incorrect.
	I was calling the `Math.atan2` method by passing the `x` value before the `y` value.
	There were two things wrong with this.
	The first is that `Math.atan2` takes the `y` value <em>before</em> the `x` value.
	The second was that, because the program exists in a space with an inverted y-axis, the y value needed to be negated for the formula to work.
	I made these changes and changed the offset angle back to `Math::PI / 2` and it worked.
</details>

<details>
	<summary>2019 - Day 12 Part 2</summary>
	Many people had trouble with this day, myself included.
	I did not figure out the LCM trick own my own, though my approach before I learned it was along the same lines.
	I also wondered if my input's answer was simply too large to compute, so I downloaded someone else's solution to see if there was a feasable answer; there was.
	I eventually figured out the two parts of keeping track of previous states of the axes.
	The first was to simply compare the first and last elements of the array, leaving the period as the size of the array minus one.
	The second was to store hashes of the states of positions AND velocities.
	Overall, I looked up more than I'm proud to admit for this challenge and feel unworthy of its star.
</details>

<details>
	<summary>2019 Day 14</summary>
	GG.

	When I first saw this problem, I had no idea how to approach it.
	Almost two hours later I was still clueless.
	It was at this point I suspected I had been defeated.
</details>


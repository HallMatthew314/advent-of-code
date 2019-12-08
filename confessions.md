# Confessions

**READER BEWARE: THIS FILE CONTAINS SPOILERS FOR SOME OF THE PROBLEMS IN ADVENT OF CODE.
THEY ARE PLACED INSIDE** `details` **TAGS BUT YOUR BROWSER MAY NOT SUPPORT THIS, LEAVING THEM VISIBLE.
READ WITH CAUTION.**

While all of the code in this repository is written by me, I cannot claim that all solutions presented are entirely my own.
What do I mean by this?
If I felt particularly stuck with a challenge, I would do a very smalla mount of research on other peoples solutions, typically on the [Advent of Code subreddit](https://old.reddit.com/r/adventofcode).
While doing so, I would avoid looking at actual code unless I *absolutley* needed to.
Instead, I tried to only read written descriptions of how other solutions worked to see I there was a piece of the puzzle I had overlooked.
I maintain that I only did this to attempt to see the problems from different angles, that I might understand them better.
Below are the problems that I have researched and what I learned from doing so.

<details>
	<summary>2015 - Day 7 Part 1</summary>
	My first major leap in progress came after I realised I could use a similar approach for this problem as I did for 2019 Day 6.
	After I implemented the hash in Ruby I ran the program, but it seemed to be taking longer than it should.
	I suspected that I was dealing with circular dependancies, as the input data is essentially a directional graph.
	I was very frustrated at this and decided to search for answers when I came across the old Daily Solutions Megathread on the subreddit.
	There I learned that after calculating a wire's value, I could replace the expression to calculate in the hash with the result.
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


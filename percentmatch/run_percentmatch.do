// Run percentmatch function on each data set

cd "~/Users/flowersa/Documents/538editing/Carl/percentmatch/final_data_sets/";

foreach file in `files' {;
	insheet using `file', comma clear;
	percentmatch, generate(pmatch)
};

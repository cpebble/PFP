


exercise1: exercise1.fut
	futhark opencl exercise1.fut
test1: exercise1
	./exercise1 -e process < exercise1.input > exercise1.output
	diff exercise1.output exercise1.output_expected

benchmark1: exercise1.fut
	futhark bench --backend=opencl --json exercise1-opencl.json exercise1.fut
	futhark bench --backend=c --json exercise1-c.json exercise1.fut
	python makeGraphs.py exercise1 test_process e1_1.png

benchmark2: exercise2.fut
	futhark bench --backend=opencl --json exercise2-opencl.json exercise2.fut
	futhark bench --backend=c --json exercise2-c.json exercise2.fut
	python makeGraphs.py exercise2 process_idx e1_2.png


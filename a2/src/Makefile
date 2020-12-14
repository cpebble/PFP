

# Generate preambles with
#target = 20000000
#toGen = 50
#import random
#generated = []
#for i in range(toGen - 1):
    #c = random.randint(target // (2*toGen), target // toGen)
    #generated += [c]
#
#generated += [target - sum(generated)]



benchmark-if:
	# generate small dataset
	echo "[true, true, true, true, true, true, false, true, false, false, false, false, false, false, false, true, true, false, true, false, false, false, false, false, false, true, false, true, true, false]\
[16i64, 12, 10, 13, 10, 9, 11, 8, 9, 11, 11, 9, 13, 13, 13, 11, 14, 14, 9, 11, 12, 8, 16, 9, 8, 10, 13, 16, 8, 173]" | futhark dataset -b > ifBench.small
	futhark dataset -b -g [500]i32 >> ifBench.small
	# Generate medium
	echo "[true, false, true, false, true, false, true, false, true, false, true]\
		[1400i64, 1299, 1879, 1760, 1762, 2000, 1315, 1125, 1053, 1909, 4498]" | futhark dataset -b > ifBench.medium
	futhark dataset -b -g [20000]i32 >> ifBench.medium
	# Generate a large ds
	echo "[false, true, true, false, true, false, false, false, true, false, true, true, false, true, true, false, true, true, true, false, false, true, false, false, true, true, false, true, true, false]\
	[26579i64, 22879, 27190, 23652, 20822, 24127, 21248, 19446, 22682, 30803, 21296, 26233, 28318, 17385, 25003, 31826, 29367, 30980, 22114, 24382, 25970, 19283, 32194, 28553, 24193, 30853, 29622, 28607, 16879, 267514]" | futhark dataset -b > ifBench.large
	futhark dataset -b -g [1000000]i32 >> ifBench.large
	# Generate a HUUGE dataset
	echo "[true, false, true, true, false, false, false, true, false, false,true, false, true, true, false, false, false, true, false, false,true, false, true, true, false, false, false, true, false, false,true, false, true, true, false, false, false, true, false, false,true, false, true, true, false, false, false, true, false, false]\
		[337168i64, 325887, 360253, 359914, 296110, 295789, 343725, 257127, 347295, 347093, 270023, 311971, 371905, 344113, 301798, 256135, 384445, 249412, 257516, 351491, 271398, 296305, 340370, 205751, 314840, 280022, 288435, 208085, 268128, 280946, 370991, 316133, 224519, 379443, 357830, 202146, 399925, 219205, 344268, 331971, 276390, 330809, 226056, 235323, 356056, 363328, 256533, 312253, 344553, 5028818]" | futhark dataset -b > ifBench.huge
	futhark dataset -b -g [20000000]i32 >> ifBench.huge
	futhark bench --backend=opencl --json flat-if-then-else-opencl.json flat-if-then-else.fut
	futhark bench --backend=c --json flat-if-then-else-c.json flat-if-then-else.fut
	python makeGraphs.py flat-if-then-else "" task2.png

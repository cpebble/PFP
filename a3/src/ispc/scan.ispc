//float inclusive_scan(float in_){
    //float tmp = exclusive_scan_add(in_);
    //// shift right
    //return shift(tmp, 1);
//}


export void scan_ispc(uniform float output[], uniform float input[], uniform int n) {
    foreach(i = 0 ... n){
    // Exclusive scan
        float in_ = input[i];
        if (programIndex == 0 && i > 0)
            in_ += output[i-1];

        float tmp = exclusive_scan_add(in_);
        output[i] = shift(tmp, 1);
        if (programIndex == programCount - 1){
            output[i] = tmp + input[i];
        }
    }
}




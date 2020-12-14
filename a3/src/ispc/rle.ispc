export uniform int rle_ispc(uniform int output[], uniform int input[], uniform int n) {
    // While to handle elements
    uniform int c = 0; // Counting offset
    uniform int m;
    uniform int cur;
    uniform int encoded = 0; // use this to handle outputting
    while(c < n){
        cur = input[c]; // Current element
        m = c + 1;
        // While to handle counting
        while(m <= (n-programCount)){
            // Increment count here
            int element = input[m+programIndex];
            if(all(element == cur)){ // If all elements match
                m += programCount; 
            } else {
                break;
            }
        }
        // Once we break we need to check the remaining in the sequence
        // A bit hackish count but who cares
        while(m < n && input[m] == cur){
            m++;
        }
        // Handle writing
        output[encoded*2] = m - c;
        output[encoded*2 + 1] = cur;
        encoded += 1;
        c = m;
    }
    return encoded*2;
}

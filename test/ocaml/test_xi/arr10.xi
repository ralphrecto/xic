main () {
    print (string_of_1array (foo()));
}

foo () : int[] {
   return {{1,2,3},{4,5}}[0] + {6, 7};
}

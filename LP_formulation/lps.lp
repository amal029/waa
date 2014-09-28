min: 
//;
TP1 + TP2 + TP3 ;

v: x1 > 0.0000001;
z: x2 > 0.0000001;
// y: x3 > 0.0000001;

c3: x1 + x2 + x3 >= 1;

c4: W1 = 2 * x1;

c6: W2 = 2 * x2;

// c8: W3 = 3 * x3;


// c10: TS = W1 + W2 + W3;
c10: TS = W1 + W2;

c2345: TP1 = 2278 * TS + 5894 * W1;

c122: TP2 = 2278 * TS + 5894 * W2;

// c13: TP3 = 5 * TS;

//X >= TP1;
//X >= TP2;
//X >= TP3;

// The meeting the reaction time problem

TP1 <= 10450 * W1;  // The reaction time for CD1 is 240
TP2 <= 10450 * W2;  // The reaction time for CD2 is 120
// TP3 <= 40 * W3;  // The reaction time for CD3 is 40


int
TS, W1, 
W2, W3, 
X, TP1, 
TP2, TP3;

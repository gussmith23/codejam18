#include <iostream>
#include <cmath>
#include <stdio.h>

int main() {
  int n;
  std::cin >> n;
  for (int i = 1; i <= n; i++) {
    double L;
    std::cin >> L;
    
    double x,y;

    x = (2*L + sqrt((4*L*L)-8*(L*L-1)))/4;
    y = L-x;

    double x1,y1,x2,y2,x3,y3;

    x1 = -(L/2);
    y1 = (L/2)-x;
    x2 = (L/2)-x;
    y2 = (L/2);
    x3 = (L/2);
    y3 = -(L/2)+x;

    printf("Case #%d:\n", i);
    printf("%.10f %.10f 0\n", (x1+x2)/2, (y1+y2)/2);
    printf("%.10f %.10f 0\n", (x2+x3)/2, (y2+y3)/2);
    printf("0 0 0.5\n");
  }
}

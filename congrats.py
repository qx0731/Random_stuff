# R
t = seq(0, 2*pi, 0.1)
x = 16*sin(t)^3
y = 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)



quartz('1234')
plot(x, y, type="n", axes=FALSE, xlab = "", ylab = "")
lines(x,y, col="red")
text(-6, 3, 'Wish you two a wonderful life ahead!')
text(4, 0, 'August 10th, 2016')

# Matlab
t = 0:0.1:2*pi;
x = 16*sin(t).^3;
y = 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t);

figure(1314)
plot(x, y, color='r')
axis equal
axis off
hold on
text(-12, 3, 'Wish you two a wonderful life ahead!')
hold on
text(0, 0, 'August 10th, 2016')

# Python
import numpy as np
import matplotlib.pyplot as plt
import math

t = np.arange(0, 2*math.pi, 0.1)
x = 16*np.sin(t)**3
y = 13*np.cos(t) - 5*np.cos(2*t) - 2*np.cos(3*t) - np.cos(4*t)

plt.plot(x, y,'r')
plt.axis('equal')
plt.axis('off')
plt.text(-12, 3, 'Wish you two a wonderful life ahead!')
plt.text(0, 0, 'August 10th, 2016')
plt.show()

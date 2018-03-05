# libtwin

C++ light tweening library

# Basic Usage

```cpp
//create a twin object, that will start from 0 and go to 100
// in 10 units of time
// using the bounceIn easing function
auto t = twin::makeTwin(0,100, 10, twin::easing::bounceIn);

//advance the tweening by one unit of time
t.step(1);

// gets the current value
int currentValue = t.get();

//gets the current progress (0 = begining, 1 = finished)
float currentProgress = t.progress();
```

# Advanced usage

```cpp
/*
creates a twin object, with a callback
this function will be called when the tweening
is over, the function takes no parameters
*/
auto t = twin::makeTwin(0,100, 0.1f, twin::easing::liear,[](){
    std::cout << "It's over !\n";
});

//reach the end, and look at the function getting called
t.step(0.1f);

```

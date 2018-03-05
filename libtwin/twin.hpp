/*
 This file is part of the Tweeny library.

 Copyright (c) 2016-2017 Leonardo G. Lucena de Freitas
 Copyright (c) 2016 Guilherme R. Costa

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 the Software, and to permit persons to whom the Software is furnished to do so,
 subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @file easing.h
 * The purpose of this file is to list all bundled easings. All easings are based on Robert Penner's easing
 * functions: http://robertpenner.com/easing/
 */


#ifndef TWIN_HPP
#define TWIN_HPP

#include <cmath>
#include <type_traits>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/**
    * @defgroup easings Easings
    * @brief Bundled easing functions based on
    *        <a href="http://robertpenner.com/easing/">Robert Penner's Easing Functions</a>
    * @details You should plug these functions into @ref tweeny::tween::via function to specify the easing used in a tween.
    * @sa tweeny::easing
    * @{
    *//**
    *   @defgroup linear Linear
    *   @{
    *       @brief The most boring ever easing function. It has no acceleration and change values in constant speed.
    *   @}
    *//**
    *   @defgroup quadratic Quadratic
    *   @{
    *       @brief The most commonly used easing functions.
    *   @}
    *//**
    *   @defgroup cubic Cubic
    *   @{
    *       @brief A bit curvier than the quadratic easing.
    *   @}
    *//**
    *   @defgroup quartic Quartic
    *   @{
    *       @brief A steeper curve. Acceleration changes faster than Cubic.
    *   @}
    *//**
    *   @defgroup quintic Quintic
    *   @{
    *       @brief An even steeper curve. Acceleration changes really fast.
    *   @}
    *//**
    *   @defgroup sinuisodal Sinuisodal
    *   @{
    *       @brief A very gentle curve, gentlier than quadratic.
    *   @}
    *//**
    *   @defgroup exponential Exponential
    *   @{
    *       @brief A very steep curve, based on the `p(t) = 2^(10*(t-1))` equation.
    *   @}
    *//**
    *   @defgroup circular Circular
    *   @{
    *       @brief A smooth, circular slope that resembles the arc of an circle.
    *   @}
    *//**
    *   @defgroup back Back
    *   @{
    *       @brief An easing function that has a "cute" natural coming back effect.
    *   @}
    *//**
    *   @defgroup elastic Elastic
    *   @{
    *       @brief An elastic easing function. Values go a little past the maximum/minimum in an elastic effect.
    *   @}
    *//**
    *   @defgroup bounce Bounce
    *   @{
    *       @brief A bouncing easing function. Values "bounce" around the maximum/minumum.
    *   @}
    *//**
    * @}
    */
namespace twin {

    /**
     * @brief The easing class holds all the bundled easings.
     *
     * You should pass the easing function to the @p tweeny::tween::via method, to set the easing function that will
     * be used to interpolate values in a tween currentPoint.
     *
     * **Example**:
     *
     * @code
     * auto tween = tweeny::from(0).to(100).via(tweeny::easing::linear);
     * @endcode
     */

enum easing{
    linear,
    sineIn, sineOut, sineInOut,
    quadIn, quadOut, quadInOut,
    cubicIn, cubicOut, cubicInOut,
    quartIn, quartOut, quartInOut,
    quintIn, quintOut, quintInOut,
    expoIn, expoOut, expoInOut,
    circIn, circOut, circInOut,
    backIn, backOut, backInOut,
    elastIn, elastOut, elastInOut,
    bounceIn, bounceOut, bounceInOut
};


namespace{
/**
 * @ingroup linear
 * @brief Values change with constant speed.
 */
template<typename T>
static typename std::enable_if<std::is_integral<T>::value, T>::type linearImpl(float position, T start, T end) {
    return static_cast<T>(roundf((end - start) * position + start));
}

template<typename T>
static typename std::enable_if<!std::is_integral<T>::value, T>::type linearImpl(float position, T start, T end) {
    return static_cast<T>((end - start) * position + start);
}

 /**
  * @ingroup quadratic
  * @brief Accelerate initial values with a quadratic equation.
  */
template<typename T>
static T quadraticIn(float position, T start, T end) {
    return static_cast<T>((end - start) * position * position + start);
}

/**
  * @ingroup quadratic
  * @brief Deaccelerate ending values with a quadratic equation.
  */
template<typename T>
static T quadraticOut(float position, T start, T end) {
    return static_cast<T>((-(end - start)) * position * (position - 2) + start);
}

/**
  * @ingroup quadratic
  * @brief Acceelerate initial and deaccelerate ending values with a quadratic equation.
  */
template<typename T>
static T quadraticInOut(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>(((end - start) / 2) * position * position + start);
    }

    --position;
    return static_cast<T>((-(end - start) / 2) * (position * (position - 2) - 1) + start);
}

/**
  * @ingroup cubic
  * @brief Aaccelerate initial values with a cubic equation.
  */
template<typename T>
static T cubicInImpl(float position, T start, T end) {
    return static_cast<T>((end - start) * position * position * position + start);
}

/**
  * @ingroup cubic
  * @brief Deaccelerate ending values with a cubic equation.
  */
template<typename T>
static T cubicOutImpl(float position, T start, T end) {
    --position;
    return static_cast<T>((end - start) * (position * position * position + 1) + start);
}

/**
  * @ingroup cubic
  * @brief Acceelerate initial and deaccelerate ending values with a cubic equation.
  */
template<typename T>
static T cubicInOutImpl(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>(((end - start) / 2) * position * position * position + start);
    }
    position -= 2;
    return static_cast<T>(((end - start) / 2) * (position * position * position + 2) + start);
}

/**
  * @ingroup quartic
  * @brief Acceelerate initial values with a quartic equation.
  */
template<typename T>
static T quarticIn(float position, T start, T end) {
    return static_cast<T>((end - start) * position * position * position * position + start);
}

/**
  * @ingroup quartic
  * @brief Deaccelerate ending values with a quartic equation.
  */
template<typename T>
static T quarticOut(float position, T start, T end) {
    --position;
    return static_cast<T>( -(end - start) * (position * position * position * position - 1) + start);
}

/**
  * @ingroup quartic
  * @brief Acceelerate initial and deaccelerate ending values with a quartic equation.
  */
template<typename T>
static T quarticInOut(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>(((end - start) / 2) * (position * position * position * position) +
                              start);
    }
    position -= 2;
    return static_cast<T>((-(end - start) / 2) * (position * position * position * position - 2) +
                          start);
}

/**
  * @ingroup quintic
  * @brief Acceelerate initial values with a quintic equation.
  */
template<typename T>
static T quinticIn(float position, T start, T end) {
    return static_cast<T>((end - start) * position * position * position * position * position + start);
}

/**
  * @ingroup quintic
  * @brief Deaccelerate ending values with a quintic equation.
  */
template<typename T>
static T quinticOut(float position, T start, T end) {
    position--;
    return static_cast<T>((end - start) * (position * position * position * position * position + 1) +
                          start);
}

/**
  * @ingroup quintic
  * @brief Acceelerate initial and deaccelerate ending values with a quintic equation.
  */
template<typename T>
static T quinticInOut(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>(
            ((end - start) / 2) * (position * position * position * position * position) +
            start);
    }
    position -= 2;
    return static_cast<T>(
        ((end - start) / 2) * (position * position * position * position * position + 2) +
        start);
}

/**
  * @ingroup sinusoidal
  * @brief Acceelerate initial values with a sinusoidal equation.
  */
template<typename T>
static T sinusoidalIn(float position, T start, T end) {
    return static_cast<T>(-(end - start) * cosf(position * static_cast<float>(M_PI) / 2) + (end - start) + start);
}

/**
  * @ingroup sinusoidal
  * @brief Deaccelerate ending values with a sinusoidal equation.
  */
template<typename T>
static T sinusoidalOut(float position, T start, T end) {
    return static_cast<T>((end - start) * sinf(position * static_cast<float>(M_PI) / 2) + start);
}

/**
  * @ingroup sinusoidal
  * @brief Acceelerate initial and deaccelerate ending values with a sinusoidal equation.
  */
template<typename T>
static T sinusoidalInOut(float position, T start, T end) {
    return static_cast<T>((-(end - start) / 2) * (cosf(position * static_cast<float>(M_PI)) - 1) + start);
}

/**
  * @ingroup exponential
  * @brief Acceelerate initial values with an exponential equation.
  */
template<typename T>
static T exponentialIn(float position, T start, T end) {
    return static_cast<T>((end - start) * powf(2, 10 * (position - 1)) + start);
}

/**
  * @ingroup exponential
  * @brief Deaccelerate ending values with an exponential equation.
  */
template<typename T>
static T exponentialOut(float position, T start, T end) {
    return static_cast<T>((end - start) * (-powf(2, -10 * position) + 1) + start);
}

/**
  * @ingroup exponential
  * @brief Acceelerate initial and deaccelerate ending values with an exponential equation.
  */
template<typename T>
static T exponentialInOut(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>(((end - start) / 2) * powf(2, 10 * (position - 1)) + start);
    }
    --position;
    return static_cast<T>(((end - start) / 2) * (-powf(2, -10 * position) + 2) + start);
}

/**
  * @ingroup circular
  * @brief Acceelerate initial values with a circular equation.
  */
template<typename T>
static T circularIn(float position, T start, T end) {
    return static_cast<T>( -(end - start) * (sqrtf(1 - position * position) - 1) + start );
}

/**
  * @ingroup circular
  * @brief Deaccelerate ending values with a circular equation.
  */
template<typename T>
static T circularOut(float position, T start, T end) {
    --position;
    return static_cast<T>((end - start) * (sqrtf(1 - position * position)) + start);
}

/**
  * @ingroup circular
  * @brief Acceelerate initial and deaccelerate ending values with a circular equation.
  */
template<typename T>
static T circularInOut(float position, T start, T end) {
    position *= 2;
    if (position < 1) {
        return static_cast<T>((-(end - start) / 2) * (sqrtf(1 - position * position) - 1) + start);
    }

    position -= 2;
    return static_cast<T>(((end - start) / 2) * (sqrtf(1 - position * position) + 1) + start);
}

/**
  * @ingroup bounce
  * @brief Deaccelerate ending values with a "bounce" equation.
  */
template<typename T>
static T bounceOutImpl(float position, T start, T end) {
    T c = end - start;
    if (position < (1 / 2.75f)) {
        return static_cast<T>(c * (7.5625f * position * position) + start);
    } else if (position < (2.0f / 2.75f)) {
        float postFix = position -= (1.5f / 2.75f);
        return static_cast<T>(c * (7.5625f * (postFix) * position + .75f) + start);
    } else if (position < (2.5f / 2.75f)) {
        float postFix = position -= (2.25f / 2.75f);
        return static_cast<T>(c * (7.5625f * (postFix) * position + .9375f) + start);
    } else {
        float postFix = position -= (2.625f / 2.75f);
        return static_cast<T>(c * (7.5625f * (postFix) * position + .984375f) + start);
    }
}

/**
  * @ingroup bounce
  * @brief Acceelerate initial values with a "bounce" equation.
  */
template<typename T>
static T bounceInImpl(float position, T start, T end) {
    return (end - start) - bounceOutImpl((1 - position), T(), end) + start;
}

/**
* @ingroup bounce
* @brief Acceelerate initial and deaccelerate ending values with a "bounce" equation.
*/
template<typename T>
static T bounceInOutImpl(float position, T start, T end) {
    if (position < 0.5f) return static_cast<T>(bounceInImpl(position * 2, T(), end) * .5f + start);
    else return static_cast<T>(bounceOutImpl((position * 2 - 1), T(), end) * .5f + (end - start) * .5f + start);
}

/**
  * @ingroup elastic
  * @brief Acceelerate initial values with an "elastic" equation.
  */
template<typename T>
static T elasticIn(float position, T start, T end) {
    if (position <= 0.00001f) return start;
    if (position >= 0.999f) return end;
    float p = .3f;
    float a = end - start;
    float s = p / 4;
    float postFix =
        a * powf(2, 10 * (position -= 1)); // this is a fix, again, with post-increment operators
    return static_cast<T>(-(postFix * sinf((position - s) * (2 * static_cast<float>(M_PI)) / p)) + start);
}

/**
  * @ingroup elastic
  * @brief Deaccelerate ending values with an "elastic" equation.
  */
template<typename T>
static T elastitcOut(float position, T start, T end) {
    if (position <= 0.00001f) return start;
    if (position >= 0.999f) return end;
    float p = .3f;
    float a = end - start;
    float s = p / 4;
    return static_cast<T>(a * powf(2, -10 * position) * sinf((position - s) * (2 * static_cast<float>(M_PI)) / p) + end);
}

/**
* @ingroup elastic
* @brief Acceelerate initial and deaccelerate ending values with an "elastic" equation.
*/
template<typename T>
static T elasticInOut(float position, T start, T end) {
    if (position <= 0.00001f) return start;
    if (position >= 0.999f) return end;
    position *= 2;
    float p = (.3f * 1.5f);
    float a = end - start;
    float s = p / 4;
    float postFix;

    if (position < 1) {
        postFix = a * powf(2, 10 * (position -= 1)); // postIncrement is evil
        return static_cast<T>(-0.5f * (postFix * sinf((position - s) * (2 * static_cast<float>(M_PI)) / p)) + start);
    }
    postFix = a * powf(2, -10 * (position -= 1)); // postIncrement is evil
    return static_cast<T>(postFix * sinf((position - s) * (2 * static_cast<float>(M_PI)) / p) * .5f + end);
}

/**
  * @ingroup back
  * @brief Acceelerate initial values with a "back" equation.
  */
template<typename T>
static T backInImpl(float position, T start, T end) {
    float s = 1.70158f;
    float postFix = position;
    return static_cast<T>((end - start) * (postFix) * position * ((s + 1) * position - s) + start);
}

/**
  * @ingroup back
  * @brief Deaccelerate ending values with a "back" equation.
  */
template<typename T>
static T backOutImpl(float position, T start, T end) {
    float s = 1.70158f;
    position -= 1;
    return static_cast<T>((end - start) * ((position) * position * ((s + 1) * position + s) + 1) + start);
}

/**
* @ingroup back
* @brief Acceelerate initial and deaccelerate ending values with a "back" equation.
*/
template<typename T>
static T backInOutImpl(float position, T start, T end) {
    float s = 1.70158f;
    float t = position;
    T b = start;
    T c = end - start;
    float d = 1;
    s *= (1.525f);
    if ((t /= d / 2) < 1) return static_cast<T>(c / 2 * (t * t * (((s) + 1) * t - s)) + b);
    float postFix = t -= 2;
    return static_cast<T>(c / 2 * ((postFix) * t * (((s) + 1) * t + s) + 2) + b);
}

}

#include <functional>
#include "libtwin_global.hpp"


    template<typename T,//type of bounds
             typename U,//type of steps
             typename F = std::function<void()>>//type of callbak function
    class LIBTWINSHARED_EXPORT Twin
    {

    public:
        Twin(T from, T to, U time, easing ease, F finalCallback):
            from(from),
            to(to),
            totalTime(time),
            advance(0),
            finishCallback(finalCallback),
            easingF(getEasing(ease))
        {
        }

        Twin(T from, T to, U time, easing ease):
            from(from),
            to(to),
            totalTime(time),
            advance(0),
            finishCallback(noop),
            easingF(getEasing(ease))
        {
        }

        std::function<T(float, T,T)> getEasing(twin::easing easingType) const
        {
            switch (easingType) {
            case linear:return linearImpl<T>;
            case sineIn:return sinusoidalIn<T>;
            case sineOut:return sinusoidalOut<T>;
            case sineInOut:return sinusoidalInOut<T>;
            case quadIn:return quadraticIn<T>;
            case quadOut:return quadraticOut<T>;
            case quadInOut:return quadraticInOut<T>;
            case cubicIn:return cubicInImpl<T>;
            case cubicOut:return cubicOutImpl<T>;
            case cubicInOut:return cubicInOutImpl<T>;
            case quartIn:return quarticIn<T>;
            case quartOut:return quarticOut<T>;
            case quartInOut:return quarticInOut<T>;
            case quintIn:return quinticIn<T>;
            case quintOut:return quinticOut<T>;
            case quintInOut:return quinticInOut<T>;
            case expoIn:return exponentialIn<T>;
            case expoOut:return exponentialOut<T>;
            case expoInOut:return exponentialInOut<T>;
            case circIn:return circularIn<T>;
            case circOut:return circularOut<T>;
            case circInOut:return circularInOut<T>;
            case backIn:return backInImpl<T>;
            case backOut:return backOutImpl<T>;
            case backInOut:return backInOutImpl<T>;
            case elastIn:return elasticIn<T>;
            case elastOut:return elastitcOut<T>;
            case elastInOut:return elasticInOut<T>;
            case bounceIn:return bounceInImpl<T>;
            case bounceOut:return bounceOutImpl<T>;
            case bounceInOut:return bounceInOutImpl<T>;
            }
        }

        void step(U progress)
        {
            advance += progress;
            totalProgress = advance/static_cast<float>(totalTime);
            if(advance >= totalTime){
                advance = totalTime;
                finishCallback();
            }
        }

        T get() const
        {
            return easingF(totalProgress, from,to);
        }

        float progresss() const
        {
            return totalProgress;
        }

    private:
        std::function<void()> noop = [](){};

        T from;
        T to;

        float totalProgress;//total progress  0 = begin, 1 = finished

        easing ez;

        U totalTime;
        U advance;

        F finishCallback;

        std::function<T(float,T,T)> easingF;
    };

    template<typename T,
             typename U,
             typename F>
    Twin<T,U,F> makeTwin(T from, T to, U time, easing ez, F func)
    {
        return Twin<T,U,F>(from, to, time, ez, func);
    }

    template<typename T,
             typename U>
    Twin<T,U> makeTwin(T from, T to, U time, easing ez)
    {
        return Twin<T,U>(from,to, time, ez);
    }
}


#endif // TWIN_HPP

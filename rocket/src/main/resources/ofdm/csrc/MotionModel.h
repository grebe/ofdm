#ifndef __MOTION_MODEL_H
#define __MOTION_MODEL_H

#include <array>
#include <stdint.h>

template <std::size_t n_cars>
struct Movement {
  double dt;

  std::array<double, n_cars> position;
  std::array<double, n_cars> velocity;
  std::array<double, n_cars> acceleration;

  Movement(double dt, double spacing, double v0);

  void step(void);
};

template <std::size_t n_cars>
Movement<n_cars>::Movement(double dt, double spacing, double v0)
  : dt(dt)
{
  std::uint64_t i;
  double p0;

  for (i = 0; i < n_cars; i++) {
    p0 = spacing * i;
    position[i] = p0;
    velocity[i] = v0;
    acceleration[i] = 0.0;
  }
}

template <std::size_t n_cars>
void Movement<n_cars>::step(void)
{
  std::uint64_t i;
  double p0;

  for (i = 0; i < n_cars; i++) {
    position[i] -= dt * velocity[i] + 0.5 * dt * dt * acceleration[i];
    velocity[i] += dt * acceleration[i];
  }

  // renormalize
  p0 = position[0];
  for (i = 0; i < n_cars; i++) {
    position[i] -= p0;
  }
}


#endif /* __MOTION_MODEL_H */

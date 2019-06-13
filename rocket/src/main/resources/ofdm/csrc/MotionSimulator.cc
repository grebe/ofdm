
#include "MotionModel.h"

#include <stdio.h>
#include <svdpi.h>
#include <vpi_user.h>

static uint64_t global_counter = 0;
static uint64_t temp = 0;

static Movement<2> movement(1.0, 10.0, 1.0);

extern "C" int motion_tick
(
  unsigned char *motion_0_actuator_ready,
  unsigned char motion_0_actuator_valid,
  int motion_0_actuator_bits_acceleration,
  unsigned char motion_0_sensor_ready,
  unsigned char *motion_0_sensor_valid,
  int *motion_0_sensor_bits_frontDistance,
  int *motion_0_sensor_bits_backDistance,
  int *motion_0_sensor_bits_velocity,
  int period_cycles
)
{
  bool transact = false;
  global_counter++;

  transact = global_counter % 300000 == 0;

  *motion_0_actuator_ready = true;
  *motion_0_sensor_valid = transact;

  if (*motion_0_sensor_valid && motion_0_sensor_ready) {
    printf("Fire!\n");
    temp++;
  }

  if (*motion_0_actuator_ready && motion_0_actuator_valid) {
    movement.acceleration[1] = motion_0_actuator_bits_acceleration / 64.0;
    printf("Acceleration = %f\n", movement.acceleration[1]);
  }


  if (transact) {
    movement.step();
    printf("Position = %f\nVelocity = %f\n", movement.position[1], movement.velocity[1]);
  }

  *motion_0_sensor_bits_frontDistance = static_cast<int>(movement.position[1] * 64.0);
  *motion_0_sensor_bits_backDistance = 0;
  *motion_0_sensor_bits_velocity = static_cast<int>(movement.velocity[1] * 64.0);

  return 0;
}

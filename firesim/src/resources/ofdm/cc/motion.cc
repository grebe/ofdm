#ifdef MOTIONWIDGET_struct_guard

#include <stdlib.h>
#include <stdio.h>

#include "motion.h"

#define PERIOD 1000
#define DELTA_T 0.001

motion_t::motion_t(simif_t *sim, MOTIONWIDGET_struct *mmio_addrs, int motion_num) :
  endpoint_t(sim), mmio(mmio_addrs), motion_num(motion_num), time(0)
{
}

motion_t::~motion_t()
{
  free(this->mmio);
}

void motion_t::dynamics()
{
  sensor_t old = this->sensor;
  this->sensor.front_distance = old.front_distance - old.velocity * DELTA_T;
  this->sensor.back_distance = old.back_distance + old.velocity * DELTA_T;
  this->sensor.velocity = old.velocity + 0.5 * DELTA_T * actuator;
}

void motion_t::recv()
{
  static bool recvd = true;
  bool go = time % PERIOD == 0;
  if (go || !recvd) {
    write(this->mmio->actuator_ready, 1);
    bool valid = read(this->mmio->actuator_valid);
    if (valid) {
      actuator = read(this->mmio->actuator_bits);
      recvd = true;
    } else {
      recvd = false;
    }
  } else {
    write(this->mmio->actuator_ready, 0);
  }
}

void motion_t::send()
{
  static bool sent = true;
  bool go = time % PERIOD == 0;
  if (go || !sent) {
    write(this->mmio->sensor_valid, 1);
    bool ready = read(this->mmio->sensor_ready);
    if (ready) {
      write(this->mmio->sensor_bits_frontDistance, sensor.front_distance);
      write(this->mmio->sensor_bits_backDistance, sensor.back_distance);
      write(this->mmio->sensor_bits_velocity, sensor.velocity);
      sent = true;
    } else {
      sent = false;
    }
  }
}

void motion_t::init()
{
  time = 0;
}

void motion_t::tick()
{
  time++;
  recv();
  dynamics();
  send();
}

#endif /* MOTIONWIDGET_struct_guard */

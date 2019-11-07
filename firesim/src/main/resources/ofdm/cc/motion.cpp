#ifdef MOTIONWIDGET_struct_guard

#include "motion.h"

motion_t::motion_t(simif_t *sim, MOTIONWIDGET_struct *mmio_addrs, int motionno):
  endpoint_t(sim),
  mmio(mmio_addrs),
  num(motionno)
{}

motion_t::~motion_t()
{
  free(this->mmio);
}

void motion_t::tick()
{
  if (read(this->mmio->actuator_valid)) {
    acceleration = read(this->mmio->actuator_bits);
    write(this->mmio->actuator_ready, 1);
    write(this->mmio->actuator_ready, 0);
  }
  if (read(this->mmio->sensor_ready)) {
    // TODO: actually implement some physics lol
    write(this->mmio->sensor_bits_frontDistance, 3);
    write(this->mmio->sensor_bits_backDistance, 4);
    write(this->mmio->sensor_bits_velocity, 5);
    write(this->mmio->sensor_valid, 1);
    write(this->mmio->sensor_valid, 0);
  }
}
#endif /* MOTIONWIDGET_struct_guard */

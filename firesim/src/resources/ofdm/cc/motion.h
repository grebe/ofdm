#ifndef __MOTION_H
#define __MOTION_H

#include <stdint.h>

#ifdef MOTIONWIDGET_struct_guard
struct sensor_t
{
  int32_t front_distance;
  int32_t back_distance;
  int32_t velocity;
};

class motion_t : public endpoint_t
{
  public:
    motion_t(simif_t *sim, MOTIONWIDGET_struct *mmio_addrs, int motion_num);
    ~motion_t();
    void dynamics();
    void recv();
    void send();
    virtual void init();
    virtual void tick();
    virtual void finish() {};
    virtual bool terminate() { return false; }
    virtual int exit_code() { return 0; }

  private:
    MOTIONWIDGET_struct *mmio;
    sensor_t sensor;
    uint64_t time;
    int32_t actuator;
    int motion_num;
};
#endif /* MOTIONWIDGET_struct_guard */

#endif /* __MOTION_H */

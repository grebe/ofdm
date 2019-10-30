#ifndef __MOTION_H
#define __MOTION_H

#include <stdint.h>

#ifdef MOTIONWIDGET_struct_guard
class motion_t: public endpoint_t
{
  public:
    motion_t(simif_t*, MOTIONWIDGET_struct*, int);
    ~motion_t();
    virtual void init() {};
    virtual void tick();
    virtual bool terminate() { return false; }
    virtual int exit_code() { return 0; }

  private:
    MOTIONWIDGET_struct *mmio;
    int32_t acceleration;
    int num;

};
#endif /* MOTIONWIDGET_struct_guard */
#endif /* __MOTION_H */

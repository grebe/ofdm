package motion

import chisel3._
import chisel3.util.{HasBlackBoxInline, HasBlackBoxResource}

class MotionSimulator(users: Int) extends BlackBox // (params = Map("users" -> IntParam(users)))
  with HasBlackBoxResource
  with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())

    val motion = Flipped(Vec(users, new MotionIO))
  })

  def motion_ports(user: Int): Seq[String] = Seq(
    s"motion_${user}_actuator_ready",
    s"motion_${user}_actuator_valid",
    s"motion_${user}_actuator_bits_acceleration",
    s"motion_${user}_sensor_ready",
    s"motion_${user}_sensor_valid",
    s"motion_${user}_sensor_bits_frontDistance",
    s"motion_${user}_sensor_bits_backDistance",
    s"motion_${user}_sensor_bits_velocity",
  )

  setInline("MotionSimulator.v",
    s"""
       |import "DPI-C" function int motion_tick
       |(
       |  output bit motion_0_actuator_ready,
       |  input bit motion_0_actuator_valid,
       |  input int motion_0_actuator_bits_acceleration,
       |  input bit motion_0_sensor_ready,
       |  output bit motion_0_sensor_valid,
       |  output int motion_0_sensor_bits_frontDistance,
       |  output int motion_0_sensor_bits_backDistance,
       |  output int motion_0_sensor_bits_velocity,
       |  input int period_cycles
       |);
       |
       |module MotionSimulator(
       |  input clock,
       |  input reset,
       |  output motion_0_actuator_ready,
       |  input motion_0_actuator_valid,
       |  input [31:0] motion_0_actuator_bits_acceleration,
       |  input motion_0_sensor_ready,
       |  output motion_0_sensor_valid,
       |  output [31:0] motion_0_sensor_bits_frontDistance,
       |  output [31:0] motion_0_sensor_bits_backDistance,
       |  output [31:0] motion_0_sensor_bits_velocity
       |);
       |
       |  wire #0.1 __motion_0_actuator_valid = motion_0_actuator_valid;
       |  wire [31:0] #0.1 __motion_0_actuator_bits_acceleration = motion_0_actuator_bits_acceleration;
       |  wire #0.1 __motion_0_sensor_ready = motion_0_sensor_ready;
       |
       |  bit __motion_0_actuator_ready;
       |  bit __motion_0_sensor_valid;
       |  int __motion_0_sensor_bits_frontDistance;
       |  int __motion_0_sensor_bits_backDistance;
       |  int __motion_0_sensor_bits_velocity;
       |  reg [31:0] period_cycles = 30000;
       |
       |  initial begin
       |    $$value$$plusargs("MOTION_PERIOD_CYCLES=%d", period_cycles);
       |  end
       |
       |  wire [31:0] #0.1 __period_cycles = period_cycles;
       |
       |  assign #0.1 motion_0_actuator_ready = __motion_0_actuator_ready;
       |  assign #0.1 motion_0_sensor_valid = __motion_0_sensor_valid;
       |  assign #0.1 motion_0_sensor_bits_frontDistance = __motion_0_sensor_bits_frontDistance;
       |  assign #0.1 motion_0_sensor_bits_backDistance = __motion_0_sensor_bits_backDistance;
       |  assign #0.1 motion_0_sensor_bits_velocity = __motion_0_sensor_bits_velocity;
       |
       |  always @(posedge clock)
       |  begin
       |    if (reset)
       |    begin
       |      __motion_0_actuator_ready = 0;
       |      __motion_0_sensor_valid = 0;
       |    end
       |    else
       |    begin
       |      motion_tick(
       |        __motion_0_actuator_ready,
       |        __motion_0_actuator_valid,
       |        __motion_0_actuator_bits_acceleration,
       |        __motion_0_sensor_ready,
       |        __motion_0_sensor_valid,
       |        __motion_0_sensor_bits_frontDistance,
       |        __motion_0_sensor_bits_backDistance,
       |        __motion_0_sensor_bits_velocity,
       |        __period_cycles
       |      );
       |    end
       |
       |  end
       |
       |endmodule
     """.stripMargin)

  addResource("/ofdm/csrc/MotionSimulator.cc")
  addResource("/ofdm/csrc/MotionModel.h")
}

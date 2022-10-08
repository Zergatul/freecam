package com.zergatul.freecam;

public class FreeCamConfig {

    public static final double MinAcceleration = 5;
    public static final double DefaultAcceleration = 50;
    public static final double MaxAcceleration = 500;
    public static final double MinMaxSpeed = 5;
    public static final double DefaultMaxSpeed = 50;
    public static final double MaxMaxSpeed = 500;
    public static final double MinSlowdownFactor = 1e-9;
    public static final double DefaultSlowdownFactor = 0.01;
    public static final double MaxSlowdownFactor = 0.5;

    public double acceleration;
    public double maxSpeed;
    public double slowdownFactor;
    public boolean renderHands;
    public boolean disableInteractions;

    public FreeCamConfig() {
        acceleration = DefaultAcceleration;
        maxSpeed = DefaultMaxSpeed;
        slowdownFactor = DefaultSlowdownFactor;
    }

    public void clamp() {
        if (acceleration < MinAcceleration || acceleration > MaxAcceleration) {
            acceleration = DefaultAcceleration;
        }
        if (maxSpeed < MinMaxSpeed || maxSpeed > MaxMaxSpeed) {
            maxSpeed = DefaultMaxSpeed;
        }
        if (slowdownFactor < MinSlowdownFactor || slowdownFactor > MaxSlowdownFactor) {
            slowdownFactor = DefaultSlowdownFactor;
        }
    }
}
package com.zergatul.freecam;

public class FreeCamConfig {

    public static final double MIN_ACCELERATION = 5;
    public static final double DEFAULT_ACCELERATION = 50;
    public static final double MAX_ACCELERATION = 500;
    public static final double MIN_MAX_SPEED = 5;
    public static final double DEFAULT_MAX_SPEED = 50;
    public static final double MAX_MAX_SPEED = 500;
    public static final double MIN_SLOWDOWN_FACTOR = 1e-9;
    public static final double DEFAULT_SLOWDOWN_FACTOR = 0.01;
    public static final double MAX_SLOWDOWN_FACTOR = 0.5;

    public double acceleration;
    public double maxSpeed;
    public double slowdownFactor;
    public boolean renderHands;
    public boolean target;
    public boolean spectatorMovement;
    public boolean rememberInputState;
    public boolean showMyName;

    public FreeCamConfig() {
        acceleration = DEFAULT_ACCELERATION;
        maxSpeed = DEFAULT_MAX_SPEED;
        slowdownFactor = DEFAULT_SLOWDOWN_FACTOR;
        target = true;
    }

    public void clamp() {
        if (acceleration < MIN_ACCELERATION || acceleration > MAX_ACCELERATION) {
            acceleration = DEFAULT_ACCELERATION;
        }
        if (maxSpeed < MIN_MAX_SPEED || maxSpeed > MAX_MAX_SPEED) {
            maxSpeed = DEFAULT_MAX_SPEED;
        }
        if (slowdownFactor < MIN_SLOWDOWN_FACTOR || slowdownFactor > MAX_SLOWDOWN_FACTOR) {
            slowdownFactor = DEFAULT_SLOWDOWN_FACTOR;
        }
    }
}
package com.zergatul.freecam.ui;

// setting = a * b ^ (slider ^ c)
public abstract class ExponentialValueMapper implements ValueMapper {

    private final double a;
    private final double b;
    private final double c;

    public ExponentialValueMapper(double min, double mid, double max) {
        a = min;
        b = max / min;
        c = Math.log(Math.log(mid / min) / Math.log(max / min)) / Math.log(0.5);
    }

    @Override
    public double toSliderValue(double value) {
        return Math.pow(Math.log(value / a) / Math.log(b), 1 / c);
    }

    @Override
    public double toSettingValue(double value) {
        return a * Math.pow(b, Math.pow(value, c));
    }
}
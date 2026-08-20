package com.zergatul.freecam.ui;

public interface ValueMapper {
    double toSliderValue(double value);
    double toSettingValue(double value);
    String toDisplay(double value);
}
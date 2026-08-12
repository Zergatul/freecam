package com.zergatul.freecam.ui;

import net.minecraft.client.gui.widget.AbstractSlider;
import net.minecraft.util.text.ITextComponent;

import java.util.function.BiConsumer;

public class SliderButton extends AbstractSlider {

    private final ITextComponent message;
    private final ValueMapper mapper;
    private final BiConsumer<SliderButton, Double> setter;

    private SliderButton(int x, int y, int width, int height, ITextComponent message, ValueMapper mapper, double value, BiConsumer<SliderButton, Double> setter) {
        super(x, y, width, height, message, mapper.toSliderValue(value));
        this.message = message;
        this.mapper = mapper;
        this.setter = setter;
        updateMessage();
    }

    @Override
    protected void updateMessage() {
        setMessage(message.copy().append(": ").append(mapper.toDisplay(value)));
    }

    @Override
    protected void applyValue() {
        setter.accept(this, mapper.toSettingValue(value));
    }

    public static class Builder {

        private int x;
        private int y;
        private int width;
        private int height;
        private ITextComponent message;
        private ValueMapper mapper;
        private double value;
        private BiConsumer<SliderButton, Double> setter;

        public Builder position(int x, int y) {
            this.x = x;
            this.y = y;
            return this;
        }

        public Builder size(int width, int height) {
            this.width = width;
            this.height = height;
            return this;
        }

        public Builder message(ITextComponent message) {
            this.message = message;
            return this;
        }

        public Builder mapper(ValueMapper mapper) {
            this.mapper = mapper;
            return this;
        }

        public Builder value(double value) {
            this.value = value;
            return this;
        }

        public Builder setter(BiConsumer<SliderButton, Double> setter) {
            this.setter = setter;
            return this;
        }

        public SliderButton create() {
            return new SliderButton(x, y, width, height, message, mapper, value, setter);
        }
    }
}
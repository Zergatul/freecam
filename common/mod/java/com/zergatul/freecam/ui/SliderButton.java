package com.zergatul.freecam.ui;

import net.minecraft.client.gui.components.AbstractSliderButton;
//import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;

public class SliderButton extends AbstractSliderButton {

    private final Component message;
    private final ValueMapper mapper;
    private final BiConsumer<SliderButton, Double> setter;
    //private final BiFunction<Double, ValueMapper, Tooltip> tooltipProvider;

    private SliderButton(int x, int y, int width, int height, Component message, /*Tooltip tooltip,*/ ValueMapper mapper, double value, BiConsumer<SliderButton, Double> setter/*, BiFunction<Double, ValueMapper, Tooltip> tooltipProvider*/) {
        super(x, y, width, height, message, mapper.toSliderValue(value));
        this.message = message;
        this.mapper = mapper;
        this.setter = setter;
        //this.tooltipProvider = tooltipProvider;
        //this.setTooltip(tooltip);
        updateMessage();
    }

    @Override
    protected void updateMessage() {
        setMessage(message.copy().append(": ").append(mapper.toDisplay(value)));
        /*if (tooltipProvider != null) {
            setTooltip(tooltipProvider.apply(value, mapper));
        }*/
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
        private Component message;
        //private Tooltip tooltip;
        private ValueMapper mapper;
        private double value;
        private BiConsumer<SliderButton, Double> setter;
        //private BiFunction<Double, ValueMapper, Tooltip> tooltipProvider;

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

        public Builder message(Component message) {
            this.message = message;
            return this;
        }

        /*public Builder tooltip(Tooltip tooltip) {
            this.tooltip = tooltip;
            return this;
        }*/

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

        /*public Builder tooltipProvider(BiFunction<Double, ValueMapper, Tooltip> tooltipProvider) {
            this.tooltipProvider = tooltipProvider;
            return this;
        }*/

        public SliderButton create() {
            return new SliderButton(x, y, width, height, message, /*tooltip,*/ mapper, value, setter/*, tooltipProvider*/);
        }
    }
}
package com.zergatul.freecam.common.mixins;

import com.zergatul.freecam.common.FreeCam;
import net.minecraft.client.CameraType;
import net.minecraft.client.gui.Gui;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(Gui.class)
public abstract class MixinGui {

    @Redirect(
            method = "renderCrosshair(Lnet/minecraft/client/gui/GuiGraphics;)V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z"))
    private boolean onRenderCrosshairIsFirstPerson(CameraType cameraType) {
        return FreeCam.instance.onRenderCrosshairIsFirstPerson(cameraType);
    }
}
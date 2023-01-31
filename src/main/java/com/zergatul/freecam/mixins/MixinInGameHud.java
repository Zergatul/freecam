package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.gui.hud.InGameHud;
import net.minecraft.client.option.Perspective;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(InGameHud.class)
public abstract class MixinInGameHud {

    @Redirect(
            method = "renderCrosshair(Lnet/minecraft/client/util/math/MatrixStack;)V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/option/Perspective;isFirstPerson()Z"))
    private boolean onRenderCrosshairIsFirstPerson(Perspective cameraType) {
        return FreeCamController.instance.onRenderCrosshairIsFirstPerson(cameraType);
    }
}
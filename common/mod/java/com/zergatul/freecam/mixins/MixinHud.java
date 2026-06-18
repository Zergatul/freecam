package com.zergatul.freecam.mixins;

import com.llamalad7.mixinextras.injector.ModifyExpressionValue;
import com.zergatul.freecam.FreeCam;
import net.minecraft.client.gui.Hud;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;

@Mixin(value = Hud.class, priority = 2000)
public abstract class MixinHud {

    @ModifyExpressionValue(
            method = "extractCrosshair",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z"))
    private static boolean onRenderCrosshairModifyIsFirstPerson(boolean value) {
        return FreeCam.instance.onRenderCrosshairModifyIsFirstPerson(value);
    }
}
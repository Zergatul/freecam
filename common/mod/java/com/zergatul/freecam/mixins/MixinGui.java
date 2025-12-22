package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.mixin.ModifyMethodReturnValue;
import net.minecraft.client.gui.Gui;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;

@Mixin(value = Gui.class, priority = 2000)
public abstract class MixinGui {

    @SuppressWarnings("unused")
    @ModifyMethodReturnValue(
            method = "extractCrosshair",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/CameraType;isFirstPerson()Z"))
    private static boolean onRenderCrosshairModifyIsFirstPerson(boolean value) {
        return FreeCam.instance.onRenderCrosshairModifyIsFirstPerson(value);
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import com.zergatul.freecam.helpers.MixinGuiHelper;
import net.minecraft.client.CameraType;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(CameraType.class)
public abstract class MixinCameraType {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/CameraType;isFirstPerson()Z", cancellable = true)
    private void onIsFirstPerson(CallbackInfoReturnable<Boolean> info) {
        if (!MixinGuiHelper.insideRenderCrosshair) {
            return;
        }
        if (!FreeCamController.instance.isActive()) {
            return;
        }
        info.setReturnValue(true);
        info.cancel();
    }
}
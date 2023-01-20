package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import com.zergatul.freecam.helpers.MixinGuiHelper;
import net.minecraft.client.settings.PointOfView;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(PointOfView.class)
public abstract class MixinPointOfView {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/settings/PointOfView;isFirstPerson()Z", cancellable = true)
    private void onIsFirstPerson(CallbackInfoReturnable<Boolean> info) {
        if (!FreeCamController.instance.isActive()) {
            MixinGameRendererHelper.insideRenderItemInHand = false;
            return;
        }
        if (MixinGuiHelper.insideRenderCrosshair && FreeCamController.instance.shouldRenderCrosshair()) {
            info.setReturnValue(true);
            info.cancel();
            return;
        }
        if (MixinGameRendererHelper.insideRenderItemInHand) {
            MixinGameRendererHelper.insideRenderItemInHand = false;
            if (FreeCamController.instance.shouldRenderHands()) {
                info.setReturnValue(true);
            }
        }
    }
}
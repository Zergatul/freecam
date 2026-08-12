package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import com.zergatul.freecam.helpers.MixinGuiHelper;
import net.minecraft.client.settings.PointOfView;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(PointOfView.class)
public abstract class MixinPointOfView {

    @Inject(at = @At("HEAD"), method = "isFirstPerson()Z", cancellable = true)
    private void onIsFirstPerson(CallbackInfoReturnable<Boolean> info) {
        if (!FreeCam.INSTANCE.isActive()) {
            MixinGameRendererHelper.insideRenderItemInHand = false;
            return;
        }
        if (MixinGuiHelper.insideRenderCrosshair && FreeCam.INSTANCE.shouldRenderCrosshair()) {
            info.setReturnValue(true);
            info.cancel();
            return;
        }
        if (MixinGameRendererHelper.insideRenderItemInHand) {
            MixinGameRendererHelper.insideRenderItemInHand = false;
            if (FreeCam.INSTANCE.shouldRenderHands()) {
                info.setReturnValue(true);
            }
        }
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import com.zergatul.freecam.helpers.MixinInGameHudHelper;
import net.minecraft.client.option.Perspective;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Perspective.class)
public abstract class MixinPerspective {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/option/Perspective;isFirstPerson()Z", cancellable = true)
    private void onIsFirstPerson(CallbackInfoReturnable<Boolean> info) {
        if (!MixinInGameHudHelper.insideRenderCrosshair) {
            return;
        }
        if (!FreeCamController.instance.isActive()) {
            return;
        }
        info.setReturnValue(true);
        info.cancel();
    }
}

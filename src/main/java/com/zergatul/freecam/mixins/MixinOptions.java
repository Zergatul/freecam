package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.option.GameOptions;
import net.minecraft.client.option.SimpleOption;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(GameOptions.class)
public abstract class MixinOptions {

    @Inject(at = @At("HEAD"), method = "getBobView()Lnet/minecraft/client/option/SimpleOption;", cancellable = true)
    private void onBobView(CallbackInfoReturnable<SimpleOption<Boolean>> info) {
        if (FreeCamController.instance.isActive()) {
            info.setReturnValue(SimpleOption.ofBoolean("", false));
        }
    }
}
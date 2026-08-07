package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Entity.class)
public abstract class MixinEntityMouseTurn {

    @Inject(method = "setAngles(FF)V", at = @At("HEAD"), cancellable = true, require = 1)
    private void freecam$onSetAngles(float yaw, float pitch, CallbackInfo callback) {
        if (FreeCam.INSTANCE.onMouseTurn((Entity)(Object)this, yaw, pitch)) {
            callback.cancel();
        }
    }
}
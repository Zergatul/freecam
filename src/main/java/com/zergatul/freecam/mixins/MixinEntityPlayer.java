package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(EntityPlayer.class)
public abstract class MixinEntityPlayer {

    @Inject(method = "getPosition(F)Lnet/minecraft/util/Vec3;", at = @At("HEAD"), cancellable = true, require = 1)
    private void freecam$getTargetPosition(float partialTicks, CallbackInfoReturnable<Vec3> callback) {
        FreeCam freeCam = FreeCam.INSTANCE;
        if (freeCam.shouldOverrideCameraEntityForPicking((EntityPlayer)(Object)this)) {
            callback.setReturnValue(freeCam.getTargetPosition());
        }
    }
}
package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public abstract class MixinEntity {

    @Inject(at = @At("HEAD"), method = "getEyePosition(F)Lnet/minecraft/world/phys/Vec3;", cancellable = true)
    private void onGetEyePosition(float partialTickTime, CallbackInfoReturnable<Vec3> info) {
        FreeCam freeCam = FreeCam.instance;
        if (freeCam.shouldOverrideCameraEntityPosition((Entity) (Object) this)) {
            info.setReturnValue(new Vec3(freeCam.getX(), freeCam.getY(), freeCam.getZ()));
        }
    }

    @Inject(at = @At("HEAD"), method = "getViewVector(F)Lnet/minecraft/world/phys/Vec3;", cancellable = true)
    private void onGetViewVector(float partialTickTime, CallbackInfoReturnable<Vec3> info) {
        FreeCam freeCam = FreeCam.instance;
        if (freeCam.shouldOverrideCameraEntityPosition((Entity) (Object) this)) {
            info.setReturnValue(Entity.calculateViewVector(freeCam.getXRot(), freeCam.getYRot()));
        }
    }
}
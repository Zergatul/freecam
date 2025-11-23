package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Camera;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Camera.class)
public abstract class MixinCamera {

    @Shadow(aliases = "Lnet/minecraft/client/Camera;setRotation(FF)V")
    protected abstract void setRotation(float p_90573_, float p_90574_);

    @Shadow(aliases = "Lnet/minecraft/client/Camera;setPosition(DDD)V")
    protected abstract void setPosition(double p_90585_, double p_90586_, double p_90587_);

    // skip all position/rotation calculations, but don't skip other fields setup logic
    @Inject(
            at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/Entity;isPassenger()Z", ordinal = 0),
            method = "setup",
            cancellable = true)
    private void onSetup(Level level, Entity entity, boolean detached, boolean mirrored, float partialTicks, CallbackInfo info) {
        FreeCam controller = FreeCam.instance;
        if (FreeCam.instance.isActive()) {
            setRotation(controller.getYRot(), controller.getXRot());
            setPosition(controller.getX(), controller.getY(), controller.getZ());
            info.cancel();
        }
    }
}
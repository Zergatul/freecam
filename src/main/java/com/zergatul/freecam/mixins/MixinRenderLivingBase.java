package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.entity.RenderLivingBase;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(RenderLivingBase.class)
public abstract class MixinRenderLivingBase {

    @Inject(
            method = "canRenderName(Lnet/minecraft/entity/EntityLivingBase;)Z",
            at = @At("RETURN"),
            cancellable = true)
    private void onCanRenderName(EntityLivingBase entity, CallbackInfoReturnable<Boolean> info) {
        if (FreeCam.instance.shouldShowMyName() && entity == Minecraft.getMinecraft().player) {
            info.setReturnValue(true);
        }
    }

    @Redirect(
            method = "renderName(Lnet/minecraft/entity/EntityLivingBase;DDD)V",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/entity/EntityLivingBase;getDistanceSq(Lnet/minecraft/entity/Entity;)D"))
    private double onRenderNameGetDistance(EntityLivingBase entity, Entity cameraEntity) {
        FreeCam freeCam = FreeCam.instance;
        if (freeCam.shouldShowMyName() && entity == Minecraft.getMinecraft().player && entity == cameraEntity) {
            double dx = entity.posX - freeCam.getX();
            double dy = entity.posY - freeCam.getY();
            double dz = entity.posZ - freeCam.getZ();
            return dx * dx + dy * dy + dz * dz;
        }

        return entity.getDistanceSq(cameraEntity);
    }
}
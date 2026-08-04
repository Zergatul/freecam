package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.client.renderer.entity.RendererLivingEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(RendererLivingEntity.class)
public abstract class MixinRendererLivingEntity {

    @Redirect(
            method = "canRenderName(Lnet/minecraft/entity/EntityLivingBase;)Z",
            at = @At(
                    value = "FIELD",
                    target = "Lnet/minecraft/client/renderer/entity/RenderManager;livingPlayer:Lnet/minecraft/entity/Entity;",
                    opcode = Opcodes.GETFIELD))
    private Entity onCanRenderNameGetLivingPlayer(RenderManager manager) {
        if (FreeCam.INSTANCE.shouldShowMyName() &&
                manager.livingPlayer == Minecraft.getMinecraft().thePlayer) {
            return null;
        }

        return manager.livingPlayer;
    }

    @Redirect(
            method = "renderName(Lnet/minecraft/entity/EntityLivingBase;DDD)V",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/entity/EntityLivingBase;getDistanceSqToEntity(Lnet/minecraft/entity/Entity;)D"))
    private double onRenderNameGetDistance(EntityLivingBase entity, Entity cameraEntity) {
        FreeCam freeCam = FreeCam.INSTANCE;

        if (freeCam.shouldShowMyName() && entity == Minecraft.getMinecraft().thePlayer && entity == cameraEntity) {
            double dx = entity.posX - freeCam.getX();
            double dy = entity.posY - freeCam.getY();
            double dz = entity.posZ - freeCam.getZ();
            return dx * dx + dy * dy + dz * dz;
        }

        return entity.getDistanceSqToEntity(cameraEntity);
    }
}
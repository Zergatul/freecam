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
                    target = "Lnet/minecraft/client/renderer/entity/RenderManager;livingPlayer:Lnet/minecraft/entity/EntityLivingBase;",
                    opcode = Opcodes.GETFIELD),
            require = 1)
    private EntityLivingBase freecam$getLivingPlayer(RenderManager manager) {
        if (FreeCam.INSTANCE.shouldShowMyName() && manager.livingPlayer == Minecraft.getMinecraft().thePlayer) {
            return null;
        }
        return manager.livingPlayer;
    }

    @Redirect(
            method = "passSpecialRender(Lnet/minecraft/entity/EntityLivingBase;DDD)V",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/entity/EntityLivingBase;getDistanceSqToEntity(Lnet/minecraft/entity/Entity;)D"),
            require = 1)
    private double freecam$getNameDistance(EntityLivingBase entity, Entity cameraEntity) {
        if (FreeCam.INSTANCE.shouldShowMyName() && entity == Minecraft.getMinecraft().thePlayer && entity == cameraEntity) {
            return FreeCam.INSTANCE.getNameDistanceSquared(entity);
        }
        return entity.getDistanceSqToEntity(cameraEntity);
    }
}
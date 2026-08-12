package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.helpers.MixinMouseHandlerHelper;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;

@Mixin(ClientPlayerEntity.class)
public abstract class MixinClientPlayerEntity extends Entity {

    public MixinClientPlayerEntity(EntityType<?> entityType, World world) {
        super(entityType, world);
    }

    @Override
    public void turn(double yRot, double xRot) {
        if (FreeCam.INSTANCE.shouldRedirectMouseTurn() && MixinMouseHandlerHelper.insideTurnPlayer) {
            FreeCam.INSTANCE.onMouseTurn(yRot, xRot);
        } else {
            super.turn(yRot, xRot);
        }
    }
}
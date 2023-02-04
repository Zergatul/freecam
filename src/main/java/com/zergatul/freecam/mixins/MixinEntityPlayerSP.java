package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.entity.Entity;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;

@Mixin(EntityPlayerSP.class)
public abstract class MixinEntityPlayerSP extends Entity {

    public MixinEntityPlayerSP(World world) {
        super(world);
    }

    @Override
    public void turn(float yaw, float pitch) {
        if (FreeCamController.instance.isActive()) {
            FreeCamController.instance.onMouseTurn(yaw, -pitch);
        } else {
            super.turn(yaw, pitch);
        }
    }
}
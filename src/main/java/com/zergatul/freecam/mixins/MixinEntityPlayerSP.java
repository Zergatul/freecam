package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
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
    public void setAngles(float yaw, float pitch) {
        if (FreeCam.INSTANCE.onMouseTurn(yaw, -pitch)) {
            super.setAngles(yaw, pitch);
        }
    }
}
using DDDNetCore.Domain.ConnectionRequests;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infraestructure.ConnectionRequests
{
    internal class IntroductionRequestEntityTypeConfiguration : IEntityTypeConfiguration<IntroductionRequest>
    {
        public void Configure(EntityTypeBuilder<IntroductionRequest> builder)
        {
            builder.ToTable("IntroductionRequests");
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.PlayerToTargetMessage);
            builder.OwnsOne(b => b.PlayerToMiddleManMessage);
            builder.OwnsOne(b => b.MiddleManToTargetMessage);
            builder.OwnsOne(b => b.CurrentStatus);
            builder.OwnsOne(b => b.Strength);
            builder.OwnsMany(b => b.Tags);

        }
    }
}

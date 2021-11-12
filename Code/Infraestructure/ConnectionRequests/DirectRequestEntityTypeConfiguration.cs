using DDDNetCore.Domain.ConnectionRequests;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infraestructure.ConnectionRequests
{
    internal class DirectRequestEntityTypeConfiguration : IEntityTypeConfiguration<DirectRequest>
    {
        public void Configure(EntityTypeBuilder<DirectRequest> builder)
        {
            builder.ToTable("DirectRequests");
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.PlayerToTargetMessage);
            builder.OwnsOne(b => b.CurrentStatus);
        }
    }
}

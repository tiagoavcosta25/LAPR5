using DDDNetCore.Domain.Connections;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infraestructure.Connections
{
    internal class ConnectionEntityTypeConfiguration : IEntityTypeConfiguration<Connection>
    {
        public void Configure(EntityTypeBuilder<Connection> builder)
        {
            //builder.ToTable("Connections", SchemaNames.DDDSample1);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(x => x.ConnectionStrength);
            builder.OwnsMany(x => x.Tags);
        }
    }
}

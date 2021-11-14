using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Players;

namespace DDDSample1.Infrastructure.Players
{
    internal class PlayerEntityTypeConfiguration : IEntityTypeConfiguration<Player>
    {
        public void Configure(EntityTypeBuilder<Player> builder)
        {
            //builder.ToTable("Players", SchemaNames.DDDSample1);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(x => x.DateOfBirth);
            builder.OwnsOne(x => x.Email);
            builder.OwnsOne(x => x.Password);
            builder.OwnsOne(x => x.PhoneNumber);
            builder.OwnsOne(x => x.Name);
            builder.OwnsOne(x => x.EmotionalStatus);
            builder.OwnsOne(x => x.Facebook);
            builder.OwnsOne(x => x.LinkedIn);
            builder.OwnsMany(x => x.Tags);
        }
    }
}
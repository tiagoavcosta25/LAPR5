using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Profiles;

namespace DDDSample1.Infrastructure.Profiles
{
    internal class ProfileEntityTypeConfiguration : IEntityTypeConfiguration<Profile>
    {
        public void Configure(EntityTypeBuilder<Profile> builder)
        {
            //builder.ToTable("Profiles", SchemaNames.DDDSample1);
            builder.HasKey(b => b.Id);
        }
    }
}
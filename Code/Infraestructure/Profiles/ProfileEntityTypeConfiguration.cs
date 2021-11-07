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
            builder.OwnsOne(x => x.DateOfBirth);
            builder.OwnsOne(x => x.Email);
            builder.OwnsOne(x => x.PhoneNumber);
            builder.OwnsOne(x => x.Name);
            builder.OwnsOne(x => x.EmotionalStatus);
            builder.OwnsOne(x => x.Facebook);
            builder.OwnsOne(x => x.LinkedIn);
        }
    }
}
using DDDSample1.Domain.Profiles;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Profiles
{
    public class ProfileRepository : BaseRepository<Profile, ProfileId>,IProfileRepository
    {
        public ProfileRepository(DDDSample1DbContext context):base(context.Profiles)
        {
           
        }
    }
}
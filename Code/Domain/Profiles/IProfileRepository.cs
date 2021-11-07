using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Profiles
{
    public interface IProfileRepository: IRepository<Profile,ProfileId>
    {
    }
}
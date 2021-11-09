using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Players
{
    public interface IPlayerRepository: IRepository<Player,PlayerId>
    {
    }
}
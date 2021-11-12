using DDDSample1.Domain.Shared;
using System.Threading.Tasks;

namespace DDDSample1.Domain.Players
{
    public interface IPlayerRepository: IRepository<Player,PlayerId>
    {
        Task<Player> GetByEmailAsync(string email);
    }
}
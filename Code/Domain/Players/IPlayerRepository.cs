using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDSample1.Domain.Players
{
    public interface IPlayerRepository: IRepository<Player,PlayerId>
    {
        Task<Player> GetByEmailAsync(string email);

        Task<List<Player>> GetByNameAsync(string name);

        Task<List<Player>> GetByPhoneAsync(string phoneNumber);
        Task<List<Player>> GetByTagAsync(string tag);
    }
}
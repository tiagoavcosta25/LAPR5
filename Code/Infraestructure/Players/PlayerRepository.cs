using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using System.Linq;
using System.Threading.Tasks;

namespace DDDSample1.Infrastructure.Players
{
    public class PlayerRepository : BaseRepository<Player, PlayerId>,IPlayerRepository
    {

        private readonly DbSet<Player> _dbplayer;

        public PlayerRepository(DDDSample1DbContext context):base(context.Players)
        {
            _dbplayer = context.Players;
        }

        public async Task<Player> GetByEmailAsync(string email)
        {
            return await _dbplayer
                .Where(x => x.Email.address.Equals(email))
                .FirstOrDefaultAsync();
        }
    }
}
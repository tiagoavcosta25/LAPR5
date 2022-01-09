using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.Shared;

namespace DDDSample1.Infrastructure.Players
{
    public class PlayerRepository : BaseRepository<Player, PlayerId>, IPlayerRepository
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

        public async Task<List<Player>> GetByNameAsync(string name)
        {
            return await _dbplayer
                .Where(x => x.Name.name.Equals(name))
                .ToListAsync();
        }

        public async Task<List<Player>> GetByPhoneAsync(string phoneNumber)
        {
            return await _dbplayer
                .Where(x => x.PhoneNumber.phoneNumber.Equals(phoneNumber))
                .ToListAsync();
        }

        public async Task<List<Player>> GetByTagAsync(string tag)
        {
            return await _dbplayer
                .Where(x => x.Tags.Select(t => t.tagName).Contains(tag))
                .ToListAsync();
        }
        
        public async Task<List<Player>> GetSuggestions(string playerEmail)
        {
            return await _dbplayer
                .Where(x => !(x.Email.address.Equals(playerEmail)))
                .ToListAsync();
        }

        public async Task<int> GetNumberOfPlayers() {
            var temp = await _dbplayer.ToListAsync();
            return temp.Count;
        }
    }
}
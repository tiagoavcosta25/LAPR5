using DDDSample1.Domain.Players;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Players
{
    public class PlayerRepository : BaseRepository<Player, PlayerId>,IPlayerRepository
    {
        public PlayerRepository(DDDSample1DbContext context):base(context.Players)
        {
           
        }
    }
}
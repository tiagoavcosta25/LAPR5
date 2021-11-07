using Code.Domain.Connections;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Domain.Connections
{
    public class Connection : Entity<ConnectionId>, IAggregateRoot
    {

        public ConnectionStrength connectionStrength { get; private set; }

        public bool Active { get; private set; }

        private Connection()
        {
            this.Active = true;
        }

        public Connection(int connectionStrength)
        {
            this.connectionStrength = new ConnectionStrength(connectionStrength);
        }

        public void MarkAsInative()
        {
            this.Active = false;
        }

    }
}

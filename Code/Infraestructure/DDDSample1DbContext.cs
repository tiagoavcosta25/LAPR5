using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Families;
using DDDSample1.Infrastructure.Categories;
using DDDSample1.Infrastructure.Products;
using DDDSample1.Infrastructure.Players;
using DDDNetCore.Domain.Connections;
using DDDNetCore.Infraestructure.Connections;
using DDDNetCore.Domain.Missions;
using DDDNetCore.Infraestructure.Missions;
using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Infraestructure.ConnectionRequests;

namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : DbContext
    {
        public DbSet<Category> Categories { get; set; }

        public DbSet<Product> Products { get; set; }

        public DbSet<Player> Players { get; set; }

        public DbSet<Family> Families { get; set; }

        public DbSet<Connection> Connections { get; set; }

        public DbSet<Mission> Missions { get; set; }

        public DbSet<IntroductionRequest> IntroductionRequests { get; set; }

        public DbSet<DirectRequest> DirectRequests { get; set; }
        public DDDSample1DbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new CategoryEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new ProductEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PlayerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new ConnectionEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new MissionEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new IntroductionRequestEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new DirectRequestEntityTypeConfiguration());

        }
    }
}